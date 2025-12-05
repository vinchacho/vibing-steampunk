# AMDP Debugger Testing Report

**Date:** 2025-12-05
**Report ID:** 018
**Subject:** AMDP Debugger API testing with ZCL_ADT_AMDP_TEST

---

## Summary

| Item | Status |
|------|--------|
| AMDP Class Created | ✅ ZCL_ADT_AMDP_TEST in $TMP |
| Unit Tests | ✅ Created (3 test methods) |
| AMDP Debugger API | ✅ Working (endpoint responds) |
| Debug Session Start | ❌ Blocked by existing lock |

---

## AMDP Test Class

Created `ZCL_ADT_AMDP_TEST` with:

```abap
CLASS zcl_adt_amdp_test DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.

    CLASS-METHODS calc_sum
      IMPORTING VALUE(iv_n) TYPE i
      EXPORTING VALUE(ev_sum) TYPE i.

    CLASS-METHODS get_sample_data
      IMPORTING VALUE(iv_count) TYPE i
      EXPORTING VALUE(et_result) TYPE tt_result.
ENDCLASS.
```

Methods use `BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT`.

### Unit Tests

- `test_calc_sum` - Verify sum of 1..10 = 55
- `test_calc_sum_zero` - Edge case: sum of 1..0 = 0
- `test_get_sample_data` - Verify 3 rows returned

---

## AMDP Debugger API Findings

### Required Accept Header

```
application/vnd.sap.adt.amdp.dbg.startmain.v1+xml
```

Standard `application/xml` returns 406 Not Acceptable.

### API Endpoints

| Method | Endpoint | Purpose |
|--------|----------|---------|
| POST | `/sap/bc/adt/amdp/debugger/main?user={user}&cascadeMode={mode}` | Start session |
| GET | `/sap/bc/adt/amdp/debugger/main/{mainId}?timeout={sec}` | Resume/wait |
| DELETE | `/sap/bc/adt/amdp/debugger/main/{mainId}?hardStop=true` | Stop session |
| POST | `/sap/bc/adt/amdp/debugger/main/{mainId}?action={step}` | Step |

### Session Lock Issue

When starting a new AMDP debug session:

```xml
<exc:exception>
  <type id="AMDB_DBG_Failure"/>
  <properties>
    <entry key="TEXT">Debugging for user "AVINOGRADOVA" already in use</entry>
    <entry key="com.sap.adt.communicationFramework.subType">DEBUGGEE_CONTEXT_LOCKED_BY_ME</entry>
  </properties>
</exc:exception>
```

This indicates an existing AMDP debug lock at the HANA level. Resolution:
1. Use SAP GUI/Eclipse ADT to release the debug session
2. Wait for debug session timeout
3. Restart HANA debugger service (admin action)

---

## Code Fixes Applied

Updated `pkg/adt/amdp_debugger.go`:

```go
// Changed from:
Accept: "application/xml"

// To:
Accept: "application/vnd.sap.adt.amdp.dbg.startmain.v1+xml"
```

---

## Recommendations

1. **Document Debug Lock**: Add note that existing AMDP debug sessions must be released before starting new ones

2. **Add Lock Detection**: Check for `DEBUGGEE_CONTEXT_LOCKED_BY_ME` error and provide helpful message

3. **Consider Force Flag**: Investigate if there's a force-release parameter for locked sessions

4. **Test on Clean System**: Full AMDP debugging workflow should be tested on a system without existing debug locks

---

## Files Created/Modified

- `ZCL_ADT_AMDP_TEST` - AMDP test class in $TMP package
- `pkg/adt/amdp_debugger.go` - Fixed Accept header
