---
name: bootstrap-system-context
description: "Probe the connected SAP system once and cache a profile at .claude/vsp-system-profile.md that other skills consult before generating code: system ID, release and allowed ABAP syntax level, installed components, detected features, transport flow, VSP mode and safety restrictions. Use at the start of work on a new system, after switching systems, or when the user says 'set up system context', 'bootstrap the system profile', 'what does this system support'. Scope: read-only probing — the only write is the local profile file."
---

# Bootstrap System Context

Build a persistent profile of the connected SAP system so every other skill
conditions on facts instead of assumptions. This kills the worst failure
class: emitting 7.50+ syntax on a 7.40 box, proposing gCTS on a classic-CTS
system, or suggesting debugger workflows the system can't run.

## Workflow

1. Run **GetSystemInfo** — system ID, release, support package, kernel, database.
2. Run **GetFeatures** — which VSP feature groups are live (abapGit, RAP, AMDP,
   UI5, transports, ZADT_VSP WebSocket services).
3. Run **GetInstalledComponents** — SAP_BASIS / SAP_ABA versions, S/4 vs ECC
   indicators, industry add-ons.
4. Note the VSP mode and safety restrictions in effect (read-only, allowed
   packages, transportable-edit policy) — these appear in tool errors and the
   server's startup context; ask the user if unclear.
5. Derive the profile (see template) — in particular the **syntax level**:

   | SAP_BASIS release | Syntax level to assume |
   |---|---|
   | < 7.40 | Classic ABAP only — no inline declarations, no constructor expressions |
   | 7.40 | Inline `DATA()`, constructor operators (`VALUE`, `CONV`, `REDUCE`), no host expressions before SP05 |
   | 7.50–7.58 | Full modern on-prem ABAP (`@DATA` host expressions, CDS view entities from 7.55+) |
   | ABAP Cloud / BTP | ABAP Cloud language version: released APIs only, `strict` mode, no direct SELECT on SAP tables, no dynpro/SUBMIT |

6. Write the profile to **`.claude/vsp-system-profile.md`** in the working
   project (create the directory if needed). Tell the user to add this file
   to their project `.gitignore` — it names a live system.
7. Confirm the summary to the user in one short paragraph.

## Profile template

```markdown
# VSP System Profile — <SID> (generated <date>, refresh after system switch)
- System: <SID>, release <rel> SP<sp>, kernel <k>, DB <db>
- Type: <ECC | S/4 on-prem | S/4 Private Cloud | BTP ABAP Environment>
- Syntax level: <from the table above — one line of what is/isn't allowed>
- Language version for new objects: <Standard ABAP | ABAP for Cloud Development>
- Transport flow: <classic CTS | gCTS | none ($TMP only)>
- Features detected: <abapGit? RAP? AMDP? UI5? ZADT_VSP websocket?>
- VSP mode & safety: <mode; read-only?; allowed packages; transportable edits?>
- Conventions observed: <namespace prefixes seen, main dev packages if known>
```

## Rules

- **Never assert release-dependent behavior from memory** — if the profile is
  missing and you cannot probe, say so and mark the advice *unverified*.
- Other skills (abap-developer, deploy, test, abap-architect) should read the
  profile if present and run this skill first if it's missing and the task is
  release- or feature-sensitive.
- Refresh the profile when `GetSystemInfo` disagrees with it (system switch,
  upgrade) — don't silently trust a stale file.
