# Clean ABAP Rules Reference

Shared rule set for the vsp plugin's code-quality skills. Distilled from the
[SAP Clean ABAP styleguide](https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md)
plus the clean-core checks that matter for ABAP Cloud, in AI-enforceable form.

## How to apply this rule set

- **Never invent ATC findings.** An `**ATC**:` tag below means the rule is
  *objectively verifiable by a tool* — it does not mean ATC flagged this code.
  Only an actual `RunATCCheck` run produces ATC findings. Check availability
  varies by system and variant: verify with `GetATCCustomizing` before claiming
  a check exists on the target system.
- **Sort tool-verifiable findings first.** When reporting, put findings for
  ATC-tagged rules at the top. If ATC was run, label each such finding
  *confirmed by ATC* or *not raised by ATC* — the second label matters, because
  it usually means the check is missing from the active variant, not that the
  finding is wrong.
- **Never assert an object's release or API state from memory.** Whether a SAP
  class, function module, or CDS entity is released for cloud development is
  system-specific. Look it up on the target system (e.g. `RunATCCheck` with a
  cloud variant, or `RunQuery` against the release-state views where available)
  or mark the claim *unverified*.
- Rules tagged `not ATC-checkable` are still real findings — they are design
  judgments a reviewer must make; no tool will make them for you. Where noted,
  `AnalyzeABAPCode` (vsp's embedded abaplint) can flag some of them locally
  without a SAP system.
- All examples use synthetic `ZDEMO`/`$ZDEMO` identifiers. Substitute the
  system's real namespace when reviewing; never copy example names into
  production objects.

---

## Naming and literals

## RULE: use-problem-domain-names

Name variables, methods, and classes after the business concept they hold, not
after their technical type. Hungarian prefixes (`lv_`, `lt_`, `is_`) and
abbreviated names force every reader to reverse-engineer intent from usage and
hide name/value mismatches that reviews should catch.

**Do**:
```abap
DATA(open_orders)      = order_reader->read_open( customer_id ).
DATA(max_retry_count)  = 5.
CONSTANTS status_blocked TYPE zdemo_order_status VALUE 'B'.
```
**Avoid**:
```abap
DATA: lt_tab1 TYPE STANDARD TABLE OF zdemo_order,
      lv_i    TYPE i VALUE 5,
      lc_b    TYPE c LENGTH 1 VALUE 'B'.
```
**ATC**: code pal for ABAP — Prefix Notation / Naming Conventions

## RULE: no-magic-numbers-or-literals

Every literal that carries business meaning gets a named constant. A bare `'B'`
or `42` inside a condition is unsearchable and unexplainable, and nobody can
change it safely because nothing says what else depends on the same value.

**Do**:
```abap
CONSTANTS status_blocked TYPE zdemo_order_status VALUE 'B'.

IF order-status = status_blocked.
  RAISE EXCEPTION NEW zcx_demo_order_blocked( order_id = order-order_id ).
ENDIF.
```
**Avoid**:
```abap
IF order-status = 'B'.
  RAISE EXCEPTION NEW zcx_demo_order_blocked( order_id = order-order_id ).
ENDIF.
```
**ATC**: code pal for ABAP — Magic Number

---

## Declarations and expressions

## RULE: prefer-inline-declarations

Declare a variable at its first use with `DATA(...)`, not in an up-front
declaration block. Up-front blocks separate declaration from use, defeat type
inference, and leave dead variables behind after refactoring; inline
declarations make scope and type obvious at the point that matters.

**Do**:
```abap
DATA(order)      = order_reader->read_single( order_id ).
DATA(line_count) = lines( order-items ).
```
**Avoid**:
```abap
DATA order      TYPE zdemo_order.
DATA line_count TYPE i.

order      = order_reader->read_single( order_id ).
line_count = lines( order-items ).
```
**ATC**: not ATC-checkable (abaplint `prefer_inline` flags it via `AnalyzeABAPCode`)

## RULE: no-chained-declarations

Do not chain declarations with `DATA:` colon lists. A chain couples unrelated
variables into one statement, produces noisy diffs when one line changes, and
usually signals that the variables should have been inline declarations or
should not all exist in this scope.

**Do**:
```abap
DATA(order)  = order_reader->read_single( order_id ).
DATA(status) = order-status.
```
**Avoid**:
```abap
DATA: order  TYPE zdemo_order,
      status TYPE zdemo_order_status,
      helper TYPE REF TO zcl_demo_order_helper.
```
**ATC**: code pal for ABAP — Chain Declaration Usage

## RULE: no-default-key-on-internal-tables

State internal table keys explicitly — a named key, or `EMPTY KEY` when order
is irrelevant. `DEFAULT KEY` silently builds a key from all character-like
components, which is slow, surprising in sorts and comparisons, and changes
behavior whenever a field is added to the row type.

**Do**:
```abap
DATA orders TYPE SORTED TABLE OF zdemo_order
            WITH UNIQUE KEY order_id.

DATA error_messages TYPE STANDARD TABLE OF string
                    WITH EMPTY KEY.
```
**Avoid**:
```abap
DATA orders         TYPE STANDARD TABLE OF zdemo_order WITH DEFAULT KEY.
DATA error_messages TYPE STANDARD TABLE OF string      WITH DEFAULT KEY.
```
**ATC**: SAP standard ATC — Internal table with DEFAULT KEY

## RULE: use-string-templates-not-concatenate

Build strings with `|...{ }...|` templates, not `CONCATENATE` or `&&` chains.
A template shows the finished message with the variables in place; the
statement forms hide the message shape behind separator bookkeeping and invite
missing-space bugs.

**Do**:
```abap
DATA(message) = |Order { order_id } rejected: status { status } (expected { expected_status })|.
```
**Avoid**:
```abap
DATA message TYPE string.
CONCATENATE 'Order' order_id 'rejected: status' status
            INTO message SEPARATED BY space.
```
**ATC**: code pal for ABAP — Text Assembly

## RULE: prefer-is-not-to-not-is

Write `IS NOT INITIAL`, `IS NOT BOUND`, `NOT line_exists( ... )` only where the
language forces it — never `NOT ... IS INITIAL`. The prefixed negation makes
the reader hold a double negative and creates precedence ambiguity in compound
conditions.

**Do**:
```abap
IF order-customer_id IS NOT INITIAL.
  process_customer_order( order ).
ENDIF.
```
**Avoid**:
```abap
IF NOT order-customer_id IS INITIAL.
  process_customer_order( order ).
ENDIF.
```
**ATC**: code pal for ABAP — Prefer IS NOT to NOT IS

## RULE: prefer-new-to-create-object

Instantiate with the `NEW` constructor expression. `CREATE OBJECT` needs a
separate declaration, cannot compose inside expressions, and marks the code as
pre-7.40; the only remaining legitimate use is dynamic instantiation with
`CREATE OBJECT ... TYPE (name)`.

**Do**:
```abap
DATA(processor) = NEW zcl_demo_order_processor( reader = order_reader
                                                log    = application_log ).
```
**Avoid**:
```abap
DATA processor TYPE REF TO zcl_demo_order_processor.
CREATE OBJECT processor
  EXPORTING reader = order_reader
            log    = application_log.
```
**ATC**: code pal for ABAP — Prefer NEW to CREATE OBJECT

## RULE: functional-call-style-not-call-method

Call methods functionally — `result = object->method( ... )` — never with
`CALL METHOD` or a `RECEIVING` clause where a functional call works. The
statement forms are longer, cannot nest, and exist only for dynamic method
names.

**Do**:
```abap
DATA(total) = calculator->calculate_total( order_id ).
```
**Avoid**:
```abap
CALL METHOD calculator->calculate_total
  EXPORTING order_id = order_id
  RECEIVING total    = total.
```
**ATC**: code pal for ABAP — CALL Method Usage

## RULE: avoid-obsolete-statements

Replace obsolete statements with their modern expression equivalents: `MOVE`
with `=`, `MOVE-CORRESPONDING` in new code with `CORRESPONDING #( )`,
`COMPUTE`, `ADD`, `SUBTRACT` with plain arithmetic, `TRANSLATE` with
`to_upper( )`/`to_lower( )`. Obsolete forms carry legacy conversion behavior
the modern reader no longer expects.

**Do**:
```abap
target = source.
total  = total + line-amount.
DATA(upper_name) = to_upper( customer_name ).
```
**Avoid**:
```abap
MOVE source TO target.
ADD line-amount TO total.
TRANSLATE customer_name TO UPPER CASE.
```
**ATC**: code pal for ABAP — Deprecated Key Words

---

## Control flow and table reads

## RULE: prefer-case-to-elseif

When branches discriminate on a single variable, use `CASE`. An
`IF ... ELSEIF` chain hides the fact that the branches are mutually exclusive
on one value, degrades with every added case, and has no equivalent of
`WHEN OTHERS` to force completeness.

**Do**:
```abap
CASE order-status.
  WHEN status_open.    process_open_order( order ).
  WHEN status_blocked. raise_blocked( order ).
  WHEN OTHERS.
    RAISE EXCEPTION NEW zcx_demo_unknown_status( status = order-status ).
ENDCASE.
```
**Avoid**:
```abap
IF order-status = 'O'.
  process_open_order( order ).
ELSEIF order-status = 'B'.
  raise_blocked( order ).
ENDIF.
```
**ATC**: code pal for ABAP — Prefer CASE to ELSEIF

## RULE: keep-nesting-shallow

Keep nesting to at most two or three levels. Deep `IF`/`LOOP` pyramids exceed
what a reviewer can hold in mind; flatten them with guard clauses
(`IF ... RETURN`), early `RAISE`, or by extracting the inner block into a
named method.

**Do**:
```abap
METHOD process_order.
  IF order-items IS INITIAL.
    RETURN.
  ENDIF.
  LOOP AT order-items INTO DATA(item).
    process_item( item ).
  ENDLOOP.
ENDMETHOD.
```
**Avoid**:
```abap
METHOD process_order.
  IF order IS NOT INITIAL.
    IF order-items IS NOT INITIAL.
      LOOP AT order-items INTO DATA(item).
        IF item-quantity > 0.
          IF item-material IS NOT INITIAL.
            process_item( item ).
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDMETHOD.
```
**ATC**: code pal for ABAP — Nesting Depth

## RULE: use-table-expressions-not-read-table

Read table lines with table expressions (`itab[ ... ]`), `line_exists( )`, and
`line_index( )` — not `READ TABLE ... WITH KEY` followed by `IF sy-subrc = 0`.
The old form is two statements where one will do, leaks `sy-subrc` into the
surrounding logic, and silently does nothing when the check is forgotten.

**Do**:
```abap
IF line_exists( orders[ order_id = order_id ] ).
  process( orders[ order_id = order_id ] ).
ENDIF.

TRY.
    DATA(order) = orders[ order_id = order_id ].
  CATCH cx_sy_itab_line_not_found.
    RAISE EXCEPTION NEW zcx_demo_order_not_found( order_id = order_id ).
ENDTRY.
```
**Avoid**:
```abap
DATA order TYPE zdemo_order.
READ TABLE orders INTO order WITH KEY order_id = order_id.
IF sy-subrc = 0.
  process( order ).
ENDIF.
```
**ATC**: code pal for ABAP — Prefer LINE_EXISTS to READ TABLE or LOOP AT

---

## Method design

## RULE: methods-do-one-thing-and-stay-small

A method does one thing, does it completely, and stays short enough to grasp
at a glance — as a working ceiling, three to five statements of orchestration
or one cohesive computation. A method doing several things cannot be named
honestly, cannot be tested in isolation, and is where bugs hide.

**Do**:
```abap
METHOD release_order.
  validate_order( order ).
  reserve_stock( order ).
  send_release_event( order ).
ENDMETHOD.
```
**Avoid**:
```abap
METHOD release_order.
  IF order-quantity <= 0.
    RAISE EXCEPTION NEW zcx_demo_invalid_quantity( order_id = order-order_id ).
  ENDIF.
  SELECT SINGLE * FROM zdemo_stock WHERE material = @order-material INTO @DATA(stock).
  IF stock-quantity < order-quantity.
    RAISE EXCEPTION NEW zcx_demo_out_of_stock( ).
  ENDIF.
  UPDATE zdemo_stock SET quantity = quantity - @order-quantity
                     WHERE material = @order-material.
  " ... plus event publishing, logging, notification ...
ENDMETHOD.
```
**ATC**: code pal for ABAP — Method Length

## RULE: at-most-three-importing-parameters

Keep methods to three importing parameters or fewer. Longer signatures almost
always mean the method has more than one responsibility, and ABAP's optional
parameters make long signatures easy to call subtly wrong — a forgotten
optional simply defaults, silently.

**Do**:
```abap
METHODS schedule_delivery
  IMPORTING order_id       TYPE zdemo_order_id
            requested_date TYPE d
            carrier        TYPE zdemo_carrier_selection.
```
**Avoid**:
```abap
METHODS schedule_delivery
  IMPORTING order_id        TYPE zdemo_order_id
            requested_date  TYPE d
            carrier_id      TYPE zdemo_carrier_id
            carrier_service TYPE zdemo_carrier_service
            allow_partial   TYPE abap_bool OPTIONAL
            allow_express   TYPE abap_bool OPTIONAL
            requested_by    TYPE syuname OPTIONAL.
```
**ATC**: code pal for ABAP — Number of Method Parameters

## RULE: prefer-returning-to-exporting

A method with one output declares it `RETURNING`. That enables functional call
style and inline declarations at the call site, and eliminates the whole class
of "forgot to read the EXPORTING parameter" bugs. Multiple outputs are a smell:
return a structure, or split the method.

**Do**:
```abap
METHODS calculate_total
  IMPORTING order_id     TYPE zdemo_order_id
  RETURNING VALUE(total) TYPE zdemo_total
  RAISING   zcx_demo_order_not_found.

DATA(total) = calculator->calculate_total( order_id ).
```
**Avoid**:
```abap
METHODS calculate_total
  IMPORTING order_id TYPE zdemo_order_id
  EXPORTING total    TYPE zdemo_total
            currency TYPE zdemo_currency
            line_cnt TYPE i.
```
**ATC**: code pal for ABAP — Prefer RETURNING to EXPORTING

## RULE: split-methods-instead-of-boolean-input

A boolean importing parameter that switches the method's behavior means the
method is two methods. Callers pass an opaque `abap_true` whose meaning is
invisible at the call site; splitting gives each variant an honest name and a
simpler body.

**Do**:
```abap
order_publisher->publish( order ).
order_publisher->publish_with_retry( order ).
```
**Avoid**:
```abap
METHODS publish
  IMPORTING order      TYPE zdemo_order
            with_retry TYPE abap_bool DEFAULT abap_false.

order_publisher->publish( order = order with_retry = abap_true ).
```
**ATC**: code pal for ABAP — Boolean Input Parameter

---

## Error handling

## RULE: class-based-exceptions-not-sy-subrc

Signal failure with class-based exceptions, never with `sy-subrc`-style return
codes or exporting flag parameters. Exceptions carry context (attributes,
message, `previous`), force the caller to decide, and survive refactoring;
a returned code is invisible at the call site and silently zero when unset.

**Do**:
```abap
METHODS read_single
  IMPORTING order_id      TYPE zdemo_order_id
  RETURNING VALUE(result) TYPE zdemo_order
  RAISING   zcx_demo_order_not_found.

TRY.
    DATA(order) = order_reader->read_single( order_id ).
  CATCH zcx_demo_order_not_found INTO DATA(error).
    RAISE EXCEPTION NEW zcx_demo_processing_failed( previous = error ).
ENDTRY.
```
**Avoid**:
```abap
order_reader->read_single(
  EXPORTING order_id = order_id
  IMPORTING result   = DATA(order)
            ev_subrc = DATA(subrc) ).
IF subrc <> 0. RETURN. ENDIF.
```
**ATC**: SAP standard ATC — Check on SY-SUBRC after specific statements

## RULE: catch-specific-exceptions-not-cx-root

Catch exactly the exception classes the code is prepared to handle. A
`CATCH cx_root` swallows programming errors and resource failures along with
the expected case, hiding in production the bug a test would have surfaced.

**Do**:
```abap
TRY.
    DATA(order) = order_reader->read_single( order_id ).
  CATCH zcx_demo_order_not_found INTO DATA(not_found).
    log_warning( not_found->get_text( ) ).
  CATCH zcx_demo_order_locked INTO DATA(locked).
    retry_later( locked->lock_owner ).
ENDTRY.
```
**Avoid**:
```abap
TRY.
    DATA(order) = order_reader->read_single( order_id ).
  CATCH cx_root.
    " what failed? we will never know
ENDTRY.
```
**ATC**: SAP standard ATC — CATCH for too generic exception class CX_ROOT

## RULE: no-empty-catch-blocks

Every `CATCH` block does something: handle, log, map to a domain exception, or
— in the rare legitimate ignore case — carry a comment naming why ignoring is
correct. An empty catch converts a failure into silent wrong behavior, the
most expensive bug class to diagnose.

**Do**:
```abap
TRY.
    cleanup_temp_data( order_id ).
  CATCH zcx_demo_cleanup_failed INTO DATA(error).
    log->warning( error->get_text( ) ).  " best-effort cleanup, processing continues
ENDTRY.
```
**Avoid**:
```abap
TRY.
    cleanup_temp_data( order_id ).
  CATCH zcx_demo_cleanup_failed.
ENDTRY.
```
**ATC**: code pal for ABAP — Empty Catch

---

## Class design

## RULE: final-classes-and-private-members-by-default

Declare classes `FINAL` and members `PRIVATE` unless there is a designed
reason not to. Non-final classes invite inheritance nobody planned for; public
attributes create couplings that can never be removed. Every relaxation should
be a decision, not the default.

**Do**:
```abap
CLASS zcl_demo_order_processor DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_demo_order_processor.
    CLASS-METHODS create
      RETURNING VALUE(result) TYPE REF TO zif_demo_order_processor.

  PRIVATE SECTION.
    DATA reader TYPE REF TO zif_demo_order_reader.
ENDCLASS.
```
**Avoid**:
```abap
CLASS zcl_demo_order_processor DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA reader TYPE REF TO zif_demo_order_reader.
    METHODS validate.
    METHODS reserve_stock.
ENDCLASS.
```
**ATC**: code pal for ABAP — Final Class

## RULE: depend-on-interfaces-inject-dependencies

Type dependencies as `REF TO zif_...` and hand them in through the constructor
(or a create method) instead of instantiating collaborators inline or reaching
for singletons. Injected interface dependencies are what make a class testable
with doubles; a hard-wired `NEW zcl_...` deep inside a method is untestable by
construction.

**Do**:
```abap
METHODS constructor
  IMPORTING reader TYPE REF TO zif_demo_order_reader
            log    TYPE REF TO zif_demo_log.
```
**Avoid**:
```abap
METHOD process.
  DATA(reader) = NEW zcl_demo_order_reader( ).   " hard-wired, untestable
  DATA(log)    = zcl_demo_log=>get_instance( ).  " singleton, shared state
  ...
ENDMETHOD.
```
**ATC**: not ATC-checkable

## RULE: comments-explain-why-not-what

Write comments that record intent, constraints, and non-obvious reasons — never
comments that restate what the next line visibly does. A "what" comment is
noise the moment it is written and a lie after the first refactor; code that
needs one usually needs a better name or an extracted method instead. Use `"`
comments; reserve `*` for commented-out history you are about to delete.

**Do**:
```abap
" Carrier API rejects same-day requests after 15:00 local — shift to next day.
IF requested_date = current_date AND current_time > carrier_cutoff.
  requested_date = next_working_day( requested_date ).
ENDIF.
```
**Avoid**:
```abap
" check if requested date equals current date and time is greater than cutoff
IF requested_date = current_date AND current_time > carrier_cutoff.
  " set requested date to next working day
  requested_date = next_working_day( requested_date ).
ENDIF.
```
**ATC**: not ATC-checkable

---

## Testing

## RULE: no-database-access-in-unit-tests

Unit tests never read or write the real database. A test that depends on rows
in the test system breaks on the next refresh, cannot run against a clean
tenant, and can corrupt shared data. Isolate with CDS test doubles
(`cl_cds_test_environment`), SQL doubles (`cl_osql_test_environment`), or
injected interface doubles.

**Do**:
```abap
METHOD reject_unknown_customer.
  cds_test_environment->insert_test_data(
    VALUE zdemo_i_customer( ( customer = 'C1000' ) ) ).
  DATA(result) = cut->validate_customer( 'C9999' ).
  cl_abap_unit_assert=>assert_false( result ).
ENDMETHOD.
```
**Avoid**:
```abap
METHOD reject_unknown_customer.
  " depends on live data in the test system — breaks on refresh
  SELECT SINGLE FROM zdemo_customer FIELDS customer
    WHERE customer = 'C1000' INTO @DATA(exists).
  cl_abap_unit_assert=>assert_not_initial( exists ).
ENDMETHOD.
```
**ATC**: code pal for ABAP — Database Access within Unit Tests

---

## ABAP Cloud / clean core

These rules apply when the target package uses the ABAP for Cloud Development
language version (BTP ABAP Environment, or S/4HANA with cloud language scope).
Confirm the system context first (`GetSystemInfo`, `GetFeatures`) rather than
assuming it — and never assert an individual object's release state from
memory: verify on the system or mark it *unverified*.

## RULE: released-apis-only

Use only SAP objects with a released (C1) API contract. An unreleased class,
function module, or CDS entity fails the cloud syntax check or ATC, and SAP
may change or delete it in any release — code built on it breaks without
warning.

**Do**:
```abap
" released API for UUIDs
DATA(uuid) = cl_system_uuid=>create_uuid_x16_static( ).
```
**Avoid**:
```abap
" unreleased internal function module — no contract, fails cloud ATC
CALL FUNCTION 'GUID_CREATE'
  IMPORTING ev_guid_16 = DATA(guid).
```
**ATC**: SAP standard ATC — Usage of released APIs (variant `ABAP_CLOUD_DEVELOPMENT_DEFAULT`)

## RULE: no-direct-select-on-sap-owned-tables

Read SAP data through released CDS view entities, never by selecting directly
from SAP-owned tables. Table structure and semantics are not part of the
public contract and shift between releases; the released views are the
supported, stable access path.

**Do**:
```abap
SELECT FROM I_SalesOrder
       FIELDS SalesOrder, OverallSDProcessStatus
       WHERE SoldToParty = @customer_id
       INTO TABLE @DATA(orders).
```
**Avoid**:
```abap
SELECT FROM vbak
       FIELDS vbeln, gbstk
       WHERE kunnr = @customer_id
       INTO TABLE @DATA(orders).
```
**ATC**: SAP standard ATC — Released database tables and views (variant `ABAP_CLOUD_DEVELOPMENT_DEFAULT`)

## RULE: abap-cloud-language-scope-only

Stay inside the ABAP for Cloud Development language scope: no classic dynpro
(`CALL SCREEN`), no `SUBMIT`, no `FORM`/`PERFORM`, no direct `CALL
TRANSACTION`, no unreleased runtime services. These constructs bind the code
to the SAP GUI stack and either fail activation (BTP) or fail cloud ATC
(on-prem cloud scope) — and the same source may still compile in classic
scope, which is why the check must run against the package's language version.

**Do**:
```abap
CLASS zcl_demo_runner DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.

CLASS zcl_demo_runner IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    out->write( |Hello from ABAP Cloud| ).
  ENDMETHOD.
ENDCLASS.
```
**Avoid**:
```abap
CALL SCREEN 100.
SUBMIT zdemo_report WITH p_custid = customer_id AND RETURN.
PERFORM process_order USING order_id.
```
**ATC**: SAP standard ATC — ABAP Language Version: ABAP for Cloud Development (variant `ABAP_CLOUD_DEVELOPMENT_DEFAULT`)

## RULE: no-modification-of-sap-standard

Never modify SAP standard objects or clone SAP source into the customer
namespace. Both break the upgrade contract and clean core; on BTP modification
is not even possible. Extend only through released extension points: Cloud
BAdIs, key-user extensibility, custom CDS views over released SAP entities,
and released RAP extension points.

**Do**:
```abap
" implement a released Cloud BAdI at the extension point SAP provides
CLASS zcl_demo_order_priority DEFINITION
  PUBLIC FINAL CREATE PUBLIC
  FOR BADI badi_demo_order_priority.
  PUBLIC SECTION.
    INTERFACES if_demo_order_priority.
ENDCLASS.
```
**Avoid**:
```abap
" copying SAP source into ZDEMO_* to 'adjust' it — breaks every upgrade
" or inheriting from an unreleased SAP class to override its behavior
CLASS zcl_demo_order_hack DEFINITION
  INHERITING FROM cl_internal_order_processor.  " SAP-internal, not released
ENDCLASS.
```
**ATC**: SAP standard ATC — Modification check / use of released enhancement options only

## RULE: interface-entity-required-annotations

Every CDS interface entity declares its contract annotations explicitly:
`@AccessControl.authorizationCheck` (a missing check is an authorization hole,
not a default), `@EndUserText.label`, and the `@AbapCatalog` flags that pin
filter and key handling. Use `define view entity` — never legacy
`define view` with `@AbapCatalog.sqlViewName`, which is forbidden in ABAP
Cloud.

**Do**:
```abap
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Demo Order - Interface'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZDEMO_I_ORDER
  as select from zdemo_order
{
  key order_uuid as OrderUUID,
      order_id   as OrderID,
      customer   as Customer
}
```
**Avoid**:
```abap
" no authorization check, no label, no catalog flags —
" every consumer bypasses authority and tooling shows a raw name
define view entity ZDEMO_I_ORDER
  as select from zdemo_order
{
  key order_uuid as OrderUUID,
      order_id   as OrderID
}
```
**ATC**: SAP standard ATC — CDS mandatory annotations

---

Adapted from [matt1as/claude-abap-skills](https://github.com/matt1as/claude-abap-skills) (Apache-2.0).
