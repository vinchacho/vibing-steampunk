---
name: rap-bo-design
description: "Structured design interview for a new RAP business object — entities, compositions with cardinality, typed fields, semantic keys, readOnly/withDraft/withoutDraft — then scaffold the full stack on the connected system with VSP tools: tables, CDS interface/projection/consumption views, behavior definition, behavior pool, service definition and binding. Use when the user says things like 'design a RAP BO', 'create a RAP business object for travel bookings', 'scaffold an OData service', 'I need a Fiori-ready managed BO with draft'. Triggers: RAP, business object, BO design, draft, behavior definition, BDEF, service definition, service binding, OData, managed, scaffold. Scope: design + first scaffold with read-back review — ongoing behavior implementation belongs to abap-developer; dependency and impact analysis to abap-architect."
---

> **System profile:** before generating release-specific syntax or proposing transport/feature workflows, read `.claude/vsp-system-profile.md` if it exists; if it's missing and the task is release- or feature-sensitive, run the **bootstrap-system-context** skill first.

You design a complete RAP business object from a short spec, then create it on the connected SAP system with VSP MCP tools. VSP has no one-shot RAP generator — the scaffold is built object by object, in dependency order, and **activation is the gate, not the write**. A stack left half-activated is worse than no stack.

RAP is release-sensitive: `strict ( 2 );`, CDS view entities, and draft handling assume a modern release (on-prem ≥ 7.55 / BTP). If the system profile says otherwise, stop and tell the user what won't work instead of scaffolding anyway.

## Tool routing

| Step | Primary vsp tool | Fallback |
|------|------------------|----------|
| Preflight: release + feature check | GetSystemInfo, GetFeatures | `.claude/vsp-system-profile.md` / ask the user |
| Confirm target package exists | GetPackage | SearchObject; CreatePackage only if the user wants a new one |
| Name-collision check | SearchObject | GrepPackages |
| Release state of referenced SAP entities | GetAPIReleaseState (URI from SearchObject) | mark the claim *unverified* — never assert from memory |
| Transport selection (non-`$` packages) | ListTransports / GetUserTransports | CreateTransport — only after the user explicitly picks "new" |
| Persistent + draft tables | CreateTable | ask the user to create in ADT, then continue |
| CDS views (DDLS) | WriteSource (`object_type: DDLS`, `mode: create`) | CreateObject (`DDLS/DF`) + WriteSource update |
| Behavior definition (BDEF) | WriteSource (`object_type: BDEF`) | CreateObject (`BDEF/BDO`) |
| Behavior pool class | CreateClassWithTests (scaffolds class + test include) | WriteSource (`object_type: CLAS`, `test_source` param) |
| Service definition (SRVD) | WriteSource (`object_type: SRVD`) | CreateObject (`SRVD/SRV`) |
| Service binding (SRVB) | CreateObject (`SRVB/SVB`, with `service_definition`, `binding_version`, `binding_category`) | none — WriteSource does not support SRVB |
| Activate the stack | ActivateMultiple (one request, SAP resolves mutual deps) | ActivatePackage; single Activate only for isolated fixes |
| Confirm nothing left inactive | GetInactiveObjects | — |
| Publish OData endpoint | PublishServiceBinding | UnpublishServiceBinding to roll back |
| Read-back for the design review | GetSource (DDLS / BDEF / SRVD / SRVB / CLAS) | none — without read-back the review rows are *not verified* |
| Quality gate | SyntaxCheck, AnalyzeABAPCode, RunUnitTests, RunATCCheck (+ GetATCCustomizing for variant availability) | — |
| Targeted post-scaffold fixes | EditSource, PrettyPrint | — |

CLI alternative for batch deployment from local files: the `vsp` DSL importer with RAP ordering (`dsl.Import(...).RAPOrder()` — DDLS → BDEF → classes → SRVD), useful when the stack already exists as files.

## Step 1 — Design interview

Collect all of the following before creating anything. Ask in **one bundled turn**, not an interrogation. If something is ambiguous, ask — do not silently pick a default.

1. **Project name** — short label driving all object names (e.g. `Travel`, `WorkOrder`).
2. **Namespace prefix** — e.g. `ZDEMO` → table `ZDEMO_TRAVEL`, views `ZDEMO_I_TRAVEL` / `ZDEMO_R_TRAVEL` / `ZDEMO_C_TRAVEL`, behavior pool `ZBP_DEMO_TRAVEL`.
3. **Application type** — `readOnly`, `withDraft`, or `withoutDraft` (transactional). Default for editable BOs is `withDraft`.
4. **Entities** — for each:
   - Alias (e.g. `Travel`, `Booking`)
   - Composition parent (if a child) and cardinality (`[0..1]` / `[0..*]`)
   - Field list: name, type (`uuid`, `char(n)`, `numc(n)`, `int`, `dec(p,s)`, `date`, `timestamp`, `amount`+currency field, `quantity`+unit field, `string`, `boolean`), and whether it is the **semantic key**
5. **References to other BOs** (customer, product, …) — these become **associations to released CDS entities**, never compositions and never direct SELECTs on SAP tables.
6. **Package** — `$TMP`/`$ZDEMO` for throwaway work, or a real transportable package (needs a transport — ask, per the hard rules).
7. **UI or Web API** — decides whether a `ZDEMO_C_*` consumption view with `@UI` annotations is needed and sets the SRVB `binding_category` (`1` = UI, `0` = Web API) and `binding_version` (`V4` preferred for new services).

Design rules to enforce while shaping the spec (RAP-specific; general clean-core rules live in [../clean-abap-review/references/rules.md](../clean-abap-review/references/rules.md) — see `released-apis-only`, `no-direct-select-on-sap-owned-tables`, `abap-cloud-language-scope-only`, `interface-entity-required-annotations`):

- **managed-vs-unmanaged** — new BO on new tables → `managed`. `unmanaged` is only for wrapping legacy persistence that cannot be migrated. Never propose `unmanaged` for greenfield.
- **semantic-key-alongside-technical-uuid** — every entity gets a `uuid` technical key (`numbering : managed, readonly`) **plus** at least one human-readable semantic key field. A UUID alone is operationally unusable.
- **composition-for-children-association-for-references** — composition only where the parent owns the child's lifecycle (lock, draft, auth follow the root). Everything else is an association.
- **cds-layering** — interface view (`_I_`, pure data shape, all mandatory annotations) → projection (`_R_`, `provider contract transactional_query`, structural mapping only, redirected compositions, **no business logic**) → consumption (`_C_`, UI annotations live here and nowhere else).
- **draft-with-correct-lock** — `with draft;` requires a draft table per entity, `lock master total etag`, `etag master`, and the draft actions (`Edit`, `Activate optimized`, `Discard`, `Resume`, `Prepare`).
- **separate-determinations-validations-side-effects** — determinations compute, never reject; validations reject, never write; side effects only declare UI refresh dependencies. Scaffold them as named TODOs in the BDEF, not as merged pseudo-methods.

Echo the finished design back as a table (entity → parent → cardinality → key fields → field list) and get a **yes** before Step 2.

## Step 2 — Preflight

1. **GetSystemInfo** + **GetFeatures** — confirm release and that RAP-relevant features are on. Cross-check `.claude/vsp-system-profile.md`.
2. **GetPackage** on the target package — confirm it exists; **SearchObject** on every planned object name — confirm nothing collides.
3. **GetAPIReleaseState** on every referenced SAP CDS entity (URI via SearchObject). Record the result per reference. If the lookup fails, the design keeps the reference but the report marks it *unverified*.
4. Transport: for non-`$` packages run **ListTransports** / **GetUserTransports**, present the candidates, and let the user choose (or explicitly request **CreateTransport**). Never auto-select.

## Step 3 — Scaffold in dependency order

Build order (matches the vsp RAP deploy order — tables → DDLS → BDEF → classes → SRVD → SRVB):

1. **Persistent table(s)** — `CreateTable`, one per entity: `client` + `<entity>_uuid` (UUID) keys, semantic key field, admin fields (`created_by/at`, `last_changed_by/at`, `local_last_changed_at`).
2. **Draft table(s)** (withDraft only) — `CreateTable`, mirroring the entity fields plus the draft admin data (DDIC include `SYCH_BDL_DRAFT_ADMIN_INC`). CreateTable's simple JSON field model cannot express DDIC includes — if listing the admin fields explicitly doesn't activate, create the draft table in ADT and record that in the report.
3. **Interface views** (`ZDEMO_I_*`) — `WriteSource` DDLS. Root view carries `composition [0..*] of` its children; children carry `association to parent`. All mandatory annotations per `interface-entity-required-annotations`.
4. **Projection views** (`ZDEMO_R_*`) — `WriteSource` DDLS. `provider contract transactional_query`, redirected compositions, nothing else.
5. **Consumption view** (`ZDEMO_C_*`, UI services only) — `WriteSource` DDLS with `@UI` annotations. (Metadata extensions — DDLX — are not creatable via vsp; if the user prefers annotations in a DDLX, that is an ADT follow-up.)
6. **Behavior definition** — `WriteSource` BDEF on the root interface view: `managed implementation in class zbp_demo_<name> unique; strict ( 2 );` (+ `with draft;`), field controls, mandatory semantic key, named TODO determinations/validations/side effects.
7. **Projection BDEF** — `WriteSource` BDEF on the projection root (`projection;` + `use` clauses).
8. **Behavior pool class** — `CreateClassWithTests` (`FOR BEHAVIOR OF`), empty handler methods matching the BDEF, plus a local ABAP Unit test class skeleton using `cl_cds_test_environment`.
9. **Service definition** — `WriteSource` SRVD exposing the projection (or consumption) entities.
10. **Service binding** — `CreateObject` `SRVB/SVB` with `service_definition`, `binding_version`, `binding_category` from the interview.

**Mid-stack syntax states are normal.** A BDEF written before its behavior pool exists legitimately fails a syntax check — do not treat per-object SyntaxCheck failures between steps 6–8 as errors to "fix". The gate comes next.

## Step 4 — Activate, verify, publish

1. **ActivateMultiple** with the whole stack in one request — SAP resolves the mutual dependencies that one-by-one activation cannot.
2. **GetInactiveObjects** — must come back clean. If anything is left inactive, fix and re-activate; do not proceed to publish with a half-activated stack.
3. **PublishServiceBinding** — publish the SRVB; report the service URL from the result. If publish fails, `UnpublishServiceBinding` + report; never leave the state ambiguous.
4. **RunUnitTests** on the behavior pool — the scaffolded tests must at least run (even if they only assert the skeleton).

## Step 5 — Read-back design review

**The review only counts if GetSource read-back succeeds.** Read the BDEF, both DDLS layers, the SRVD, and the behavior pool back with **GetSource** — every row below is a claim about source you have actually re-read from the system, not about what you intended to write. If a read-back fails, mark that row *not verified* instead of ✓.

| Check | How verified |
|-------|--------------|
| BDEF has `strict ( 2 );` | GetSource(BDEF) |
| `@AccessControl.authorizationCheck: #CHECK` on every interface entity | GetSource(DDLS) |
| Semantic key present and marked mandatory; UUID is `readonly` managed numbering | GetSource(DDLS + BDEF) |
| Composition only for owned children; references are associations to **released** entities | GetSource(DDLS) + GetAPIReleaseState |
| Projection views contain no business logic (no joins, cases, calculations) | GetSource(DDLS) |
| Draft: draft tables + `lock master total etag` + draft actions all present (withDraft only) | GetSource(BDEF) |
| Determinations / validations / side effects scaffolded as separate named TODOs | GetSource(BDEF) |
| ABAP Unit skeleton with CDS test doubles present | GetSource(CLAS, include=testclasses) |
| Everything active | GetInactiveObjects |

Then run **RunATCCheck** on the package (confirm variant/check availability with **GetATCCustomizing** first) and **AnalyzeABAPCode** on the behavior pool. In the findings list, **sort tool-verifiable findings first** and label each one *confirmed by ATC* or *not raised by ATC* — the second label usually means the check is absent from the active variant, not that the finding is wrong.

## Hard rules

- **Never assert an object's release or API state from memory.** Look it up with GetAPIReleaseState (or RunQuery against release-state views where available) or mark the claim *unverified*.
- **Never invent ATC findings.** Only actual RunATCCheck output counts; verify a check exists on this system with GetATCCustomizing before citing it.
- **Sort tool-verifiable findings first**, each labeled *confirmed by ATC* / *not raised by ATC*.
- **Activation is the gate, not the write.** ActivateMultiple, then GetInactiveObjects clean, before publishing or reporting success.
- **Review only after read-back.** No GetSource, no ✓ — mark *not verified*.
- **Ask before transports.** Never auto-select an existing transport; never auto-create one.
- **Never propose `unmanaged` for a new BO on new tables.**
- **Every entity gets a semantic key** and every BDEF gets `strict ( 2 );` — regardless of whether the user asked.
- **Synthetic names in examples** (`ZDEMO_*`, `$ZDEMO`, `TR-EXAMPLE`); substitute the user's real namespace only in the actual system objects.
- **Stop at the scaffold.** Implementing the determination/validation/action logic, and every later edit, is **abap-developer**'s job — hand off with the report below.

## Output format

```
# RAP BO Design — <Project>

## Decision summary
- Implementation type: managed — <one sentence why>
- Application type: readOnly | withDraft | withoutDraft — <why>
- Service: OData <V2|V4>, <UI|Web API>
- Package: <name> — <transport | $-local>

## Design (as confirmed by user)
<entity table: entity | parent | cardinality | semantic key | fields>

## Created objects
| Type | Name | Status |
|------|------|--------|
| TABL / DDLS / BDEF / CLAS / SRVD / SRVB | ... | active | inactive | failed |

## Activation & publish
<ActivateMultiple result, GetInactiveObjects result, service URL or publish failure>

## Design review (read-back verified)
<the Step 5 table with ✓ / ✗ / not verified per row + ATC summary>

## Next steps (hand off to abap-developer)
1. Implement the TODO determinations/validations/actions in ZBP_...
2. Replace the ABAP Unit skeleton asserts with real behavior tests (CDS test doubles).
3. <any ✗ / not-verified rows, DDLX or access-control follow-ups in ADT>
```

---

Adapted from [matt1as/claude-abap-skills](https://github.com/matt1as/claude-abap-skills) (Apache-2.0).
