# ABAP Unit Reference

Assertion signatures, test-class attributes, lifecycle invariants, and test-environment templates for the generate-unit-tests skill. Use these verbatim instead of recalling signatures from memory. All identifiers below are synthetic (`ZDEMO_*` / `ZCL_DEMO_*`).

## CL_ABAP_UNIT_ASSERT — assertion methods

| Method | Purpose | Example |
| --- | --- | --- |
| `assert_equals` | Value equality | `assert_equals( act = result exp = 42 )` |
| `assert_true` | Boolean true | `assert_true( act = lv_flag )` |
| `assert_false` | Boolean false | `assert_false( act = lv_flag )` |
| `assert_initial` | Value is initial | `assert_initial( act = lt_table )` |
| `assert_not_initial` | Value is not initial | `assert_not_initial( act = lt_result )` |
| `assert_bound` | Reference is bound | `assert_bound( act = lo_instance )` |
| `assert_not_bound` | Reference is not bound | `assert_not_bound( act = lo_ref )` |
| `assert_differs` | Values are different | `assert_differs( act = val1 exp = val2 )` |
| `assert_char_cp` | Character pattern match | `assert_char_cp( act = lv_text exp = '*error*' )` |
| `assert_char_np` | Character pattern no match | `assert_char_np( act = lv_text exp = '*secret*' )` |
| `assert_number_between` | Number in range | `assert_number_between( number = val lower = 1 upper = 10 )` |
| `assert_table_contains` | Table contains line | `assert_table_contains( line = wa table = lt_result )` |
| `assert_table_not_contains` | Table does not contain line | `assert_table_not_contains( line = wa table = lt_result )` |
| `assert_return_code` | sy-subrc check | `assert_return_code( act = sy-subrc exp = 0 )` |
| `fail` | Force test failure | `fail( msg = 'Should not reach here' )` |

Always pass `msg` on assertions whose failure would otherwise be ambiguous. Common patterns: assert `lines( lt_result )` for row counts; in exception tests call the method, `fail( )` if it returns, then `CATCH` the specific exception class and assert on `get_text( )`.

## Test-class attributes

| Attribute | Options | Semantics |
| --- | --- | --- |
| `DURATION` | `SHORT` / `MEDIUM` / `LONG` | Expected execution time; `SHORT` (< 1s) is the CI default. vsp `RunUnitTests` skips `LONG` tests unless `include_long=true`. |
| `RISK LEVEL` | `HARMLESS` / `DANGEROUS` / `CRITICAL` | Impact on system data; `HARMLESS` = no DB changes. vsp `RunUnitTests` skips `DANGEROUS` (and higher) unless `include_dangerous=true`. |

Test-method additions: `FOR TESTING` marks a test method; `RAISING cx_static_check` lets checked exceptions propagate (the test fails on any unhandled exception instead of failing to compile).

## Lifecycle invariants

| Fixture method | Runs | Must contain (when a test environment is used) |
| --- | --- | --- |
| `class_setup` | Once, before all tests | **Create** the environment (`...=>create( ... )`) — creation is expensive, do it once |
| `setup` | Before each test | **`environment->clear_doubles( )`** — isolates every test from the previous one's data |
| `teardown` | After each test | Per-test cleanup of CUT state if needed |
| `class_teardown` | Once, after all tests | **`environment->destroy( )`** — always, or the doubles leak into later runs |

All three environment calls (create / clear_doubles / destroy) are mandatory whenever any environment below is used. For plain doubles without an environment: create fresh CUT and doubles in `setup`.

## Test-environment templates

### 1. CDS test environment (`cl_cds_test_environment`)

For testing a CDS view entity; automatically stubs all data sources of the view (works with associations, joins, and expressions).

```abap
CLASS ltc_cds_view DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CLASS-DATA environment TYPE REF TO if_cds_test_environment.
    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.
    METHODS setup.
    METHODS test_view_calculation FOR TESTING.
ENDCLASS.

CLASS ltc_cds_view IMPLEMENTATION.
  METHOD class_setup.
    environment = cl_cds_test_environment=>create( i_for_entity = 'ZDEMO_I_SALESORDER' ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD test_view_calculation.
    " Arrange — insert test data into the stubbed data source
    DATA lt_test_data TYPE STANDARD TABLE OF zdemo_salesorder.
    lt_test_data = VALUE #(
      ( client = sy-mandt order_id = '001' customer_id = 'CUST1'
        net_amount = 100 currency_code = 'EUR' status = 'N' )
      ( client = sy-mandt order_id = '002' customer_id = 'CUST1'
        net_amount = 200 currency_code = 'EUR' status = 'A' ) ).
    environment->insert_test_data( i_data = lt_test_data ).

    " Act — select from the CDS view
    SELECT FROM zdemo_i_salesorder
      FIELDS OrderId, NetAmount, Status
      WHERE CustomerId = 'CUST1'
      INTO TABLE @DATA(lt_result).

    " Assert
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 2
      msg = 'Expected 2 orders for CUST1' ).
  ENDMETHOD.
ENDCLASS.
```

### 2. OSQL test environment (`cl_osql_test_environment`)

For testing ABAP classes whose logic runs ABAP SQL against specific tables/views.

```abap
CLASS ltc_sql_dependent DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CLASS-DATA environment TYPE REF TO if_osql_test_environment.
    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.
    METHODS setup.
    METHODS test_read_customers FOR TESTING.
ENDCLASS.

CLASS ltc_sql_dependent IMPLEMENTATION.
  METHOD class_setup.
    environment = cl_osql_test_environment=>create(
      i_dependency_list = VALUE #(
        ( 'ZDEMO_CUSTOMER' )
        ( 'ZDEMO_SALESORDER' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD test_read_customers.
    " Arrange
    DATA lt_customers TYPE STANDARD TABLE OF zdemo_customer.
    lt_customers = VALUE #(
      ( client = sy-mandt customer_id = 'C1' customer_name = 'Alice' )
      ( client = sy-mandt customer_id = 'C2' customer_name = 'Bob' ) ).
    environment->insert_test_data( i_data = lt_customers ).

    " Act
    DATA(cut) = NEW zcl_demo_customer_reader( ).
    DATA(lt_result) = cut->get_all_customers( ).

    " Assert
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 2 ).
  ENDMETHOD.
ENDCLASS.
```

### 3. RAP transactional buffer double (`cl_botd_txbufdbl_bo_test_env`)

For testing code that **consumes** a RAP BO via EML — the BO's persistence is replaced by a transactional buffer double.

```abap
CLASS ltc_rap_consumer DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CLASS-DATA environment TYPE REF TO if_botd_txbufdbl_bo_test_env.
    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.
    METHODS setup.
    METHODS test_create_order FOR TESTING.
ENDCLASS.

CLASS ltc_rap_consumer IMPLEMENTATION.
  METHOD class_setup.
    environment = cl_botd_txbufdbl_bo_test_env=>create(
      environment_config = cl_botd_txbufdbl_bo_test_env=>prepare_environment_config(
      )->set_bdef_dependencies( VALUE #( ( 'ZDEMO_R_SALESORDER' ) ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD test_create_order.
    " Act — code under test uses EML against the doubled BO
    MODIFY ENTITIES OF zdemo_r_salesorder
      ENTITY Root
      CREATE FIELDS ( Description Status )
      WITH VALUE #(
        ( %cid = 'test1'
          Description = 'Test Order'
          Status = 'NEW' ) )
      MAPPED DATA(mapped)
      FAILED DATA(failed)
      REPORTED DATA(reported).

    " Assert
    cl_abap_unit_assert=>assert_initial( act = failed ).
    cl_abap_unit_assert=>assert_not_initial( act = mapped-root ).
  ENDMETHOD.
ENDCLASS.
```

### 4. RAP mock EML API (`cl_botd_mockemlapi_bo_test_env`)

For testing RAP **handler method implementations** — EML calls the handler makes are answered by configured mock responses.

```abap
CLASS ltc_rap_handler DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CLASS-DATA environment TYPE REF TO if_botd_mockemlapi_bo_test_env.
    CLASS-METHODS class_setup.
    CLASS-METHODS class_teardown.
    METHODS setup.
    METHODS test_action_handler FOR TESTING.
ENDCLASS.

CLASS ltc_rap_handler IMPLEMENTATION.
  METHOD class_setup.
    environment = cl_botd_mockemlapi_bo_test_env=>create(
      environment_config = cl_botd_mockemlapi_bo_test_env=>prepare_environment_config(
      )->set_bdef_dependencies( VALUE #( ( 'ZDEMO_R_SALESORDER' ) ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD test_action_handler.
    " Configure mock EML API responses
    DATA lt_read_result TYPE TABLE FOR READ RESULT zdemo_r_salesorder.
    lt_read_result = VALUE #(
      ( OrderUUID = '12345' Description = 'Test' Status = 'NEW' ) ).

    environment->get_test_double( 'ZDEMO_R_SALESORDER'
      )->configure_read_response( lt_read_result ).

    " Create handler instance for testing
    DATA lo_handler TYPE REF TO lhc_root.
    CREATE OBJECT lo_handler FOR TESTING.

    " Execute handler method and assert on its result...
  ENDMETHOD.
ENDCLASS.
```

**Choosing between 3 and 4:** the code under test *calls* a RAP BO → transactional buffer double (3); the code under test *is* the BO's behavior implementation → mock EML API (4).

## Further reading

- [SAP ABAP Cheat Sheets — ABAP Unit Tests](https://github.com/SAP-samples/abap-cheat-sheets/blob/main/14_ABAP_Unit_Tests.md)
- [SAP Help — CDS Test Double Framework](https://help.sap.com/docs/abap-cloud/abap-development-tools-user-guide/cds-test-double-framework)
- [Clean ABAP — Testing](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#testing)

---

Adapted from [likweitan/abap-skills](https://github.com/likweitan/abap-skills) `abap-unit-testing` (MIT) and [arc-mcp/arc-1](https://github.com/arc-mcp/arc-1) `generate-abap-unit-test` (MIT). Identifiers replaced with synthetic `ZDEMO_*` names.
