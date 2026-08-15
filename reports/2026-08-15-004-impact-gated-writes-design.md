# Impact-Gated Writes — Design Pointer

**Date:** 2026-08-15
**Report ID:** 004
**Subject:** Blast-radius summaries on writes with an opt-in enforcement gate — pointer to the as-built design
**Related Documents:** [docs/plans/2026-08-15-impact-gated-writes-design.md](../docs/plans/2026-08-15-impact-gated-writes-design.md) (design, as built) · [docs/plans/2026-08-15-impact-gated-writes.md](../docs/plans/2026-08-15-impact-gated-writes.md) (implementation plan) · [2026-08-15-001-sap-mcp-skills-landscape-and-borrow-roadmap.md](2026-08-15-001-sap-mcp-skills-landscape-and-borrow-roadmap.md) · [2026-08-15-003-skill-content-borrow-analysis.md](2026-08-15-003-skill-content-borrow-analysis.md)

Shipped on `feature/impact-gated-writes` (2026-08-15). Every risky write
(update/edit/delete/rename) can return an `impact` block — caller count,
package spread, 90-day transport touches, a risk tier (high/medium/low/unknown),
and one agent-directed advice sentence — computed from where-used plus
E071/E070 in ~3 stateless reads, degrading to `risk: unknown` without ever
failing the write by itself.

Enforcement is two-tier. Advisory summaries attach at four workflow sites
(WriteSource, EditSource, RenameObject, DeleteObjectWithResult), computed
before lock acquisition. Block mode (`--impact-gate block`) additionally
enforces at the UpdateSource and UpdateClassInclude primitives, so markerless
routes (expert/hyperfocused tools, DeployZip, dsl.Import, WriteProgram/WriteClass)
cannot bypass the gate; create-fill writes and internal cleanup deletes are
exempt by design. A refusal carries a single-use `impact-confirm-<32 hex>`
token; retrying with `confirm` (MCP, 13 tools) or `--confirm-impact` (CLI)
proceeds, and one confirm covers a whole multi-step write, rename included.

Config: `SAP_IMPACT_GATE`/`--impact-gate` (`off`·`advise`·`block`, default
`off`) and `SAP_IMPACT_THRESHOLD`/`--impact-threshold` (`high`·`medium`,
default `high`; `medium` also gates `unknown`). Deep documentation — exemption
table, confirmation semantics, and the reviewed trade-offs (token-burn timing,
reissue races, per-retry recomputation, batch amplification) — lives in the
design doc above.
