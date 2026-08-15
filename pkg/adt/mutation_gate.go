package adt

import (
	"context"
	"fmt"
)

type mutationPackageCheckedKey struct{}

// withMutationPackageChecked marks that an outer workflow already resolved
// and checked the target package before acquiring a stateful lock. Inner
// mutators still enforce operation and transport policy, but skip the redundant
// package lookup that would otherwise insert a stateless SearchObject request
// between LOCK and PUT/DELETE.
func withMutationPackageChecked(ctx context.Context) context.Context {
	return context.WithValue(ctx, mutationPackageCheckedKey{}, true)
}

func mutationPackageAlreadyChecked(ctx context.Context) bool {
	checked, _ := ctx.Value(mutationPackageCheckedKey{}).(bool)
	return checked
}

type impactCreateFillKey struct{}

// withImpactCreateFill marks the source-fill step of a just-created object —
// blast radius is definitionally empty (a brand-new object has no callers)
// and a degraded-mode block here (risk "unknown" under threshold "medium"
// during a where-used outage) would strand a partial create between LOCK and
// PUT, the same rationale as the ungated cleanup DeleteObject. Set by the
// create-then-fill workflows (writeSourceCreate, CreateFromFile,
// CreateAndActivateProgram, CreateClassWithTests, ExecuteABAP's temp
// program); honored by the primitive-level block guards in UpdateSource and
// UpdateClassInclude, which skip impact computation when it is present.
// Mirrors withMutationPackageChecked's shape.
func withImpactCreateFill(ctx context.Context) context.Context {
	return context.WithValue(ctx, impactCreateFillKey{}, true)
}

func impactCreateFillExempt(ctx context.Context) bool {
	exempt, _ := ctx.Value(impactCreateFillKey{}).(bool)
	return exempt
}

type impactComputedKey struct{}

// impactComputedMarker is the ctx value stashed by outer write workflows: the
// blast-radius summary plus the origin identity of the mutation it was
// computed for. Step 4 of checkMutation gates only at the checkMutation whose
// (Op, ObjectURL) matches this origin — sub-steps of a multi-step workflow
// (rename's create/write/activate legs) inherit the marker but have a
// different identity and skip the gate.
//
// confirmed flips to true when a confirmation token is consumed at an
// origin-matching gate. Later origin-matching gates within the SAME logical
// write (rename step 6 deletes the old object with the origin's exact
// (OpDelete, oldURL); EditSource's inner UpdateSource canonicalizes to the
// origin URL) honor it instead of demanding a second, un-issuable token. The
// marker is per-logical-write and only touched by the goroutine driving that
// write, so the field needs no locking.
type impactComputedMarker struct {
	summary   *ImpactSummary
	op        OperationType
	objectURL string
	confirmed bool
}

// withImpactComputed stashes the blast-radius summary an outer write workflow
// computed before acquiring its lock — together with the (op, objectURL)
// origin of that mutation — so downstream policy checks (block-mode
// enforcement) can read it without recomputing, mirroring the
// package-checked marker above.
func withImpactComputed(ctx context.Context, s *ImpactSummary, op OperationType, objectURL string) context.Context {
	return context.WithValue(ctx, impactComputedKey{}, &impactComputedMarker{
		summary:   s,
		op:        op,
		objectURL: objectURL,
	})
}

// impactMarkerFromContext returns the stashed marker, or nil when the outer
// workflow did not compute an impact (gate off, or a create-path write).
func impactMarkerFromContext(ctx context.Context) *impactComputedMarker {
	m, _ := ctx.Value(impactComputedKey{}).(*impactComputedMarker)
	return m
}

// impactFromContext returns the stashed impact summary for result attachment,
// or nil when the outer workflow did not compute one.
func impactFromContext(ctx context.Context) *ImpactSummary {
	if m := impactMarkerFromContext(ctx); m != nil {
		return m.summary
	}
	return nil
}

// MutationSurface identifies the object surface a mutation targets. Different
// surfaces require different metadata resolution strategies (ADT SearchObject,
// UI5 BSP metadata, etc.). Use SurfaceADT for standard ABAP objects.
type MutationSurface int

const (
	// SurfaceADT is the default ADT object surface (classes, programs,
	// interfaces, data elements, message classes, etc.). Package resolution
	// for existing objects uses SearchObject.
	SurfaceADT MutationSurface = iota

	// SurfaceUI5 is the UI5/BSP filestore surface. Package resolution for
	// existing UI5 apps is not yet implemented — mutations on this surface
	// are blocked when AllowedPackages is configured until app→package
	// resolution lands.
	SurfaceUI5
)

// MutationContext describes a single mutation operation for policy evaluation.
// Callers should build a MutationContext at the top of every mutating public
// method and pass it to checkMutation before performing any side effects.
//
// There are two ways to provide the target package:
//   - For **existing** objects: set ObjectURL and leave Package empty. The
//     gate resolves the package from the object metadata.
//   - For **create** operations: set Package explicitly (and optionally
//     ObjectURL, but it will not be resolved).
type MutationContext struct {
	// Op is the operation type used for safety whitelist/blacklist checks
	// (OpCreate, OpUpdate, OpDelete, OpActivate, OpWorkflow, ...).
	Op OperationType

	// OpName is a human-readable name of the operation, used in error
	// messages ("EditSource", "DeleteObject", ...).
	OpName string

	// ObjectURL is the ADT URL of an existing object being mutated. When
	// AllowedPackages is configured and Package is empty, the gate resolves
	// the object's package via this URL.
	ObjectURL string

	// Package is an explicit target package, used for create operations
	// where the package is a caller-supplied parameter.
	Package string

	// Transport is the transport request number supplied by the caller
	// (empty for local objects).
	Transport string

	// Surface selects the package-resolution strategy. Defaults to SurfaceADT.
	Surface MutationSurface
}

// checkMutation runs all policy checks for a mutation operation in a single
// place. It performs (in order):
//
//  1. Operation-type safety check (read-only, allowed/disallowed ops)
//  2. Package ownership check (resolves from ObjectURL for existing objects,
//     or uses explicit Package for creates)
//  3. Transportable-edit check (when a transport is supplied)
//  4. Impact gate (block mode): refuses at/above the configured risk
//     threshold until confirmed with a token (WithImpactConfirm)
//
// This is the single source of truth for mutation policy. Individual mutators
// should call this at the top of their implementation instead of wiring the
// sub-checks by hand — that avoids the class of bug where one sub-check is
// forgotten and a whole mutation path silently bypasses policy.
func (c *Client) checkMutation(ctx context.Context, m MutationContext) error {
	// 1. Operation type check
	if err := c.checkSafety(m.Op, m.OpName); err != nil {
		return err
	}

	// 2. Package ownership check. Outer workflows may mark only this networked
	// portion as complete; operation and transport checks are never bypassed.
	if !mutationPackageAlreadyChecked(ctx) {
		if err := c.checkMutationPackage(ctx, m); err != nil {
			return err
		}
	}

	// 3. Transportable-edit check
	if err := c.checkTransportableEdit(m.Transport, m.OpName); err != nil {
		return err
	}

	// 4. Impact gate (block mode)
	if err := c.checkImpactGate(ctx, m); err != nil {
		return err
	}

	return nil
}

// checkImpactGate is step 4 of checkMutation: block-mode enforcement of the
// blast-radius summary an outer workflow stashed in ctx. It gates ONLY at the
// checkMutation whose (Op, ObjectURL) matches the marker's origin — object
// URLs compared canonicalized — so the sub-step gates of a multi-step
// workflow (rename's create/write/activate legs) skip it and a single
// confirmation suffices for the whole logical write.
func (c *Client) checkImpactGate(ctx context.Context, m MutationContext) error {
	// Allowlist compare, deliberately not `!= off`: an unrecognized gate
	// value that slipped past config normalization must stay inert.
	if c.config.Safety.ImpactGate != ImpactGateBlock {
		return nil
	}
	marker := impactMarkerFromContext(ctx)
	if marker == nil || marker.summary == nil {
		return nil
	}
	if m.Op != marker.op {
		return nil
	}
	target := canonicalizeObjectURL(m.ObjectURL)
	if target == "" || target != canonicalizeObjectURL(marker.objectURL) {
		return nil
	}

	risk := marker.summary.Risk
	blocked := risk == riskHigh ||
		(c.config.Safety.ImpactThreshold == ImpactThresholdMedium && (risk == riskMedium || risk == riskUnknown))
	if !blocked {
		return nil
	}

	// A token already consumed at an earlier origin-matching gate of this
	// same logical write (rename step 6, EditSource's inner UpdateSource)
	// keeps authorizing it.
	if marker.confirmed {
		return nil
	}
	opKey := impactTokenOp(m.Op)
	if token := impactConfirmFromContext(ctx); token != "" && c.consumeImpactToken(m.ObjectURL, opKey, token) {
		marker.confirmed = true
		return nil
	}

	// Refuse — and issue a fresh token INSIDE the blocked path so every
	// block response carries a valid retry (reissue overwrites any prior
	// token for this key).
	object := marker.summary.Object
	if object == "" {
		object = m.ObjectURL
	}
	return &ImpactBlockedError{
		Summary: marker.summary,
		Token:   c.IssueImpactToken(m.ObjectURL, opKey),
		Object:  object,
		Op:      impactOpVerb(m.Op),
	}
}

// checkMutationPackage validates the target package for a mutation. If no
// package whitelist is configured, the check is a no-op.
func (c *Client) checkMutationPackage(ctx context.Context, m MutationContext) error {
	if len(c.config.Safety.AllowedPackages) == 0 {
		return nil
	}

	// If the caller supplied an explicit package (create path), check it
	// directly.
	if m.Package != "" {
		return c.checkPackageSafety(m.Package)
	}

	// Otherwise resolve the package from the existing object.
	if m.ObjectURL == "" {
		return fmt.Errorf("mutation gate: %s requires either ObjectURL or Package when AllowedPackages is configured", m.OpName)
	}

	switch m.Surface {
	case SurfaceADT:
		return c.checkObjectPackageSafety(ctx, m.ObjectURL)

	case SurfaceUI5:
		// UI5 app→package resolution is not yet implemented. Fail closed
		// when a package whitelist is configured so that UI5 mutations do
		// not silently bypass policy.
		return fmt.Errorf(
			"operation '%s' on UI5 surface is blocked: UI5 app→package resolution not yet implemented, cannot verify package against SAP_ALLOWED_PACKAGES (tracked as follow-up)",
			m.OpName)

	default:
		return fmt.Errorf("mutation gate: unknown surface %d for %s", m.Surface, m.OpName)
	}
}
