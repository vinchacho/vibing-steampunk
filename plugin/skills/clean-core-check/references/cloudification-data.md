# Cloudification Repository Data Reference

Authoritative lookup data for the [SAP Cloudification Repository](https://github.com/SAP/abap-atc-cr-cv-s4hc) —
the JSON files SAP publishes as content for the ATC released-API checks, the
Clean-Core Level ↔ API-state mapping, and the ATC check names that consume them.

**Authority order.** For the *current connected system*, `GetAPIReleaseState` is
the authority — it reflects what is actually released on that box. This file is
the authority for the *target release*: which APIs will be available after the
move to Cloud ERP / Private Edition. The two can disagree (an API released in
2025 FPS01 is still unreleased on a 2022 system); report both, never average
them. Never quote a JSON URL or SAP Note number from memory when this file has
it — copy it from here.

## Two independent A–D axes — never merge them

SAP defines **two separate Clean-Core Level scales that share the letters A–D**:

| Axis | SAP Note | Grades |
|---|---|---|
| **Extensibility** (custom code / API usage) | [3578329](https://me.sap.com/notes/3578329) | How compliant an extension object is with released-API and extensibility rules |
| **Integration** (interfaces) | [3690029](https://me.sap.com/notes/3690029) | How compliant an interface/integration is with released integration content |

An object can be extensibility Level A and integration Level C at the same
time. Keep the two grades as **separate fields — never collapse them into one
"clean core level"**. Everything below (and the grading scheme in SKILL.md) is
the **extensibility axis**. If the user asks about interface/integration
grading, say it is a different scale (Note 3690029) and out of this skill's
scope — do not reuse this mapping for it.

## JSON file URLs by target product

All content is served raw from `https://raw.githubusercontent.com/SAP/abap-atc-cr-cv-s4hc/main/src/`.

### SAP Cloud ERP (public cloud)

| File | URL |
|---|---|
| Released APIs (latest) | `https://raw.githubusercontent.com/SAP/abap-atc-cr-cv-s4hc/main/src/objectReleaseInfoLatest.json` |

### SAP Cloud ERP Private (PCE) — version-specific

| Version | URL |
|---|---|
| **Latest** | `https://raw.githubusercontent.com/SAP/abap-atc-cr-cv-s4hc/main/src/objectReleaseInfo_PCELatest.json` |
| Release 2025 FPS00 | `https://raw.githubusercontent.com/SAP/abap-atc-cr-cv-s4hc/main/src/objectReleaseInfo_PCE2025_0.json` |

**URL pattern:** `objectReleaseInfo_PCE{YEAR}_{FPS}.json` (e.g. `PCE2025_1`,
`PCE2023_3`, `PCE2022_2`; the 2022 FPS00 file is plain `objectReleaseInfo_PCE2022.json`).
Files exist for Feature Pack Stack releases only — there are no per-SP files.
The version list grows over time: derive the URL from the pattern and the
system's release (from `GetSystemInfo` / the system profile). If a fetch of a
derived URL fails, say "file not found for this release", do not substitute a
guessed version.

### New Clean Core checks (Note 3565942) — object classifications

| File | URL |
|---|---|
| Object Classifications (SAP) | `https://raw.githubusercontent.com/SAP/abap-atc-cr-cv-s4hc/main/src/objectClassifications_SAP.json` |
| Object Classifications (3-Tier Model) | `https://raw.githubusercontent.com/SAP/abap-atc-cr-cv-s4hc/main/src/objectClassifications_3TierModel.json` |
| Object Classifications (general) | `https://raw.githubusercontent.com/SAP/abap-atc-cr-cv-s4hc/main/src/objectClassifications.json` |

### SAP BTP, ABAP Environment

| File | URL |
|---|---|
| BTP latest | `https://raw.githubusercontent.com/SAP/abap-atc-cr-cv-s4hc/main/src/objectReleaseInfo_BTPLatest.json` |

## ATC checks that consume these files

| Target | ATC check category → check name | Content URL to enter in check attributes |
|---|---|---|
| SAP Cloud ERP | **Cloud Readiness** → *Usage of Released APIs (Cloudification Repository)* | `objectReleaseInfoLatest.json` |
| SAP Cloud ERP Private | **Clean Core** → *Usage of Released APIs (Cloudification Repository)* | `objectReleaseInfo_PCE…` for the target release |
| New Clean Core checks (Note 3565942) | *Usage of APIs* and *Allowed Enhancement Technologies* | `objectClassifications_SAP.json` |

These are SAP's check names inside a customer-maintained check variant (ATC/SCI)
— the variant name itself is site-specific. On the connected system, confirm
what actually exists with `GetATCCustomizing` before running `RunATCCheck`
(the vsp default cloud variant `ABAP_CLOUD_DEVELOPMENT_DEFAULT` is a separate,
content-URL-free check set; a variant with the cloudification check configured
is additional signal, not a replacement for the SKILL.md procedure).

## Clean-Core Level ↔ API-state mapping (extensibility axis)

SAP objects carry **states** in the JSON files; consuming custom code is graded
into **levels**:

| Level | API state in JSON | Viewer label | Meaning |
|---|---|---|---|
| **A** | `released` | Released | Allowed in ABAP Cloud ("ABAP for Cloud Development" language version) |
| **A** | `deprecated` | Deprecated | Still allowed in ABAP Cloud, but a newer recommended API exists |
| **B** | `classicAPI` | Classic API | Usable only in Standard ABAP language version, not in ABAP Cloud |
| **C** | `notToBeReleased` | Not to be released | Not permitted in ABAP Cloud; a successor API exists |
| **D** | `noAPI` | No API | Not usable in ABAP Cloud and not recommended in Standard ABAP either |
| — | `internalAPI` | Internal API | SAP-internal only; treat consumption as Level D for grading |

### Per-level remediation rules

- **Level A (`released`)**: the target. All new development consumes only these.
- **Level A (`deprecated`)**: still compliant today, but record the named
  successor and plan the migration — deprecation is a scheduled Level-C.
- **Level B (`classicAPI`)**: permitted for clean-core extensions in Standard
  ABAP only. To bridge into ABAP Cloud, wrap it — SAP's reference pattern is
  the Tier-2 RFC proxy ([SAP-samples/tier2-rfc-proxy](https://github.com/SAP-samples/tier2-rfc-proxy)).
  Never call it directly from cloud-language code.
- **Level C (`notToBeReleased`)**: migrate to the successor API listed in the
  JSON entry. Claim Level C only when the successor lookup was actually done;
  a missing successor moves the finding toward D (or *unverified* if the data
  could not be read).
- **Level D (`noAPI`)**: avoid entirely — no successor is planned; requires
  redesign, not substitution.

### State fields per JSON family

- `objectReleaseInfo*.json` (Cloud Readiness / Clean Core released-API check):
  states `released`, `deprecated`, `notToBeReleased`, `notReleased`
- `objectClassifications*.json` (Note 3565942 checks): states `classicAPI`,
  `noAPI`, `internalAPI`
- Labels on entries: `remote-enabled` (RFC-callable), `transactional-consistent`
  (commit/rollback-safe, suitable for RAP-based consumption)

## Required SAP Notes

### Cloud Readiness approach (Cloud ERP)

| Note | Purpose |
|---|---|
| [3284711](https://me.sap.com/notes/3284711) | ATC check for GitHub repository content |
| [3377462](https://me.sap.com/notes/3377462) | ATC check error fix |
| [3507814](https://me.sap.com/notes/3507814) | Support for own released objects |

### Clean Core approach (Cloud ERP Private)

| Note | Purpose |
|---|---|
| [3449860](https://me.sap.com/notes/3449860) | Classic API support in ATC checks |
| [3565942](https://me.sap.com/notes/3565942) | ATC checks "Usage of APIs" and "Allowed Enhancement Technologies" |
| [3710789](https://me.sap.com/notes/3710789) | Function-group fix for Classic APIs |
| [3470426](https://me.sap.com/notes/3470426) | Collection note (>20000 Level-A released data elements) |
| [3489660](https://me.sap.com/notes/3489660) | UI5 ABAP Repository deployment with ABAP Cloud |

The system must reach `raw.githubusercontent.com` over SSL (STRUST cert import;
handshake troubleshooting in note [3582797](https://me.sap.com/notes/3582797)).
If the note state on the connected system is unknown, report the configuration
prerequisite as *unverified* — do not assume the checks are installed.

## Cloudification API Viewer (interactive browsing)

| Product | Viewer URL |
|---|---|
| SAP Cloud ERP | https://sap.github.io/abap-atc-cr-cv-s4hc/ |
| SAP Cloud ERP Private | https://sap.github.io/abap-atc-cr-cv-s4hc/?version=objectReleaseInfo_PCELatest.json |
| Classic API Clean Core model | https://sap.github.io/abap-atc-cr-cv-s4hc/?version=objectClassifications_SAP.json |
(MIT), skill `atc-cloudification`. Two-axes rule per SAP Notes 3578329 / 3690029.
