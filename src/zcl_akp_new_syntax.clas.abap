CLASS zcl_akp_new_syntax DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES: if_oo_adt_classrun.
    TYPES:
      BEGIN OF ty_line_struct1,
        col1 TYPE char3,
        col2 TYPE char3,
      END OF ty_line_struct1,

      BEGIN OF ty_line_struct2,
        col2 TYPE char3,
        col3 TYPE char3,
      END OF ty_line_struct2,

      BEGIN OF ty_struct1,
        col1 TYPE char3,                                              "elementary components,
        col2 TYPE char3,
        tab  TYPE STANDARD TABLE OF ty_line_struct1 WITH EMPTY KEY, "tabular component
      END OF ty_struct1,

      BEGIN OF ty_struct2,
        col2 TYPE char3,
        tab  TYPE STANDARD TABLE OF ty_line_struct2 WITH EMPTY KEY,
        col4 TYPE char3,
      END OF ty_struct2.

    TYPES: tt_tab1 TYPE STANDARD TABLE OF ty_struct1 WITH EMPTY KEY,
           tt_tab2 TYPE STANDARD TABLE OF ty_struct2 WITH EMPTY KEY.
  PROTECTED SECTION.
    METHODS inline_declaration
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.
    METHODS fieldSymbol_vs_dataReference
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.
    METHODS new_vs_value
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.
    METHODS embedded_expressions
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.
    METHODS     move_corresponding
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS corresponding_operator
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS let_expression
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS for_loop
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS reduce_operator
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS group_by
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS filter_operator
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.

    METHODS open_sql_enhancements
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.
  PRIVATE SECTION.
    METHODS display_structure1
      IMPORTING
        out        TYPE REF TO if_oo_adt_classrun_out
        is_struct1 TYPE ty_struct1.

    METHODS display_structure2
      IMPORTING
        out        TYPE REF TO if_oo_adt_classrun_out
        is_struct2 TYPE ty_struct2.

    METHODS display_tab1
      IMPORTING
        out     TYPE REF TO if_oo_adt_classrun_out
        it_tab1 TYPE tt_tab1.

    METHODS display_tab2
      IMPORTING
        out     TYPE REF TO if_oo_adt_classrun_out
        it_tab2 TYPE tt_tab2.


    METHODS clear_fill_structures
      CHANGING
        cs_struct2 TYPE zcl_akp_new_syntax=>ty_struct2
        cs_struct1 TYPE zcl_akp_new_syntax=>ty_struct1.
    METHODS clear_fill_itabs
      CHANGING
        ct_tab1 TYPE zcl_akp_new_syntax=>tt_tab1
        ct_tab2 TYPE zcl_akp_new_syntax=>tt_tab2.
    METHODS new_line
      IMPORTING
        i_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS notes
      IMPORTING
        i_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS sql_cte
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.
    METHODS sql_window_expressions
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.
    METHODS sql_union
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.
    METHODS sql_having_clause
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.
    METHODS sql_groupby_detail
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.
    METHODS sql_cross_right_outer_join
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.
    METHODS sql_host_expression
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.
    METHODS sql_coalesce
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.
    METHODS sql_case
      IMPORTING
        out TYPE REF TO if_oo_adt_classrun_out.


ENDCLASS.



CLASS zcl_akp_new_syntax IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    "Shortcuts
    " Ctrl + 1 : Quick assist.
    " Ctrl + Alt + DownArrow : Create a copy of the current line

*    inline_declaration( out ).
*    fieldSymbol_vs_dataReference( out ).
*    new_vs_value( out ).
*    embedded_expressions( out ).
*    move_corresponding( out ).
*    corresponding_operator( out ).
*    let_expression( out ).
*    for_loop( out ).
*    reduce_operator( out ).
*    group_by( out ).
*    filter_operator( out ).
*    open_sql_enhancements( out ).

    "https://www.youtube.com/watch?v=4KA_s7ct1Pw
    "Corresponding COMPONENT Operator

  ENDMETHOD.


  METHOD     move_corresponding.
    "Move Corresponding
    "https://www.youtube.com/watch?v=TDHJdaf9Y0c

*MOVE-CORRESPONDING with deep structure and without using EXPANDING NESTED TABLES
*MOVE-CORRESPONDING with deep structure and using EXPANDING NESTED TABLES
*MOVE-CORRESPONDING with Nested table without using EXPANDING NESTED TABLES
*MOVE-CORRESPONDING with Nested table with using EXPANDING NESTED TABLES
*MOVE-CORRESPONDING with Nested table with using KEEPING TARGET LINES

**Example MOVE-CORRESPONDING for Structures        DEMO_MOVE_CORRESPONDING_STRUCT
**Example MOVE-CORRESPONDING for Internal Tables   DEMO_MOVE_CORRESPONDING_STRUCT
**Example Component Operator for Structures        DEMO_CORRESPONDING_STRUCT
**Example Component Operator for Internal Tables   DEMO_CORRESPONDING_ITAB

*    TYPES:
*      BEGIN OF ty_line_struct1,
*        col1 TYPE char3,
*        col2 TYPE char3,
*      END OF ty_line_struct1,
*
*      BEGIN OF ty_line_struct2,
*        col2 TYPE char3,
*        col3 TYPE char3,
*      END OF ty_line_struct2,
*      BEGIN OF ty_struct1,
*        col1 TYPE char3,                                              "elementary components,
*        col2 TYPE char3,
*        tab  TYPE STANDARD TABLE OF ty_line_struct1 WITH EMPTY KEY, "tabular component
*      END OF ty_struct1,
*
*      BEGIN OF ty_struct2,
*        col2 TYPE char3,
*        tab  TYPE STANDARD TABLE OF ty_line_struct2 WITH EMPTY KEY,
*        col4 TYPE char3,
*      END OF ty_struct2.
*
*    TYPES: tt_tab1 TYPE STANDARD TABLE OF ty_struct1 WITH EMPTY KEY,
*           tt_tab2 TYPE STANDARD TABLE OF ty_struct2 WITH EMPTY KEY.

*Important
    "In case of itabs if any column matches than , first target itab is cleared
    "KEEPING TARGET LINES will not clear the target itab but will append to it

    DATA:
      ls_struct1 TYPE ty_struct1,
      ls_struct2 TYPE ty_struct2.


    clear_fill_structures(
      CHANGING
        cs_struct2 = ls_struct2
        cs_struct1 = ls_struct1 ).

    me->display_structure1( out = out is_struct1 = ls_struct1 ).
    me->display_structure2( out = out is_struct2 = ls_struct2 ).

    out->write( '>>MOVE-CORRESPONDING ls_struct1 TO ls_struct2--- ' ).
    MOVE-CORRESPONDING ls_struct1 TO ls_struct2.
    me->display_structure2( out = out is_struct2 = ls_struct2 ).


    clear_fill_structures(
      CHANGING
        cs_struct2 = ls_struct2
        cs_struct1 = ls_struct1 ).

    out->write( '>>MOVE-CORRESPONDING ls_struct1 TO ls_struct2 EXPANDING NESTED TABLES--- ' ).
    MOVE-CORRESPONDING ls_struct1 TO ls_struct2 EXPANDING NESTED TABLES.
    me->display_structure2( out = out is_struct2 = ls_struct2 ).

    DATA: lt_tab1 TYPE  tt_tab1,
          lt_tab2 TYPE tt_tab2.

    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).

    out->write( '===============Begin TABLE section===============' ).
    display_tab1( it_tab1 = lt_tab1 out = out ).
    display_tab2( it_tab2 = lt_tab2 out = out ).

    MOVE-CORRESPONDING lt_tab1 TO lt_tab2.
    out->write( '>>MOVE-CORRESPONDING lt_tab1 to lt_tab2.' ).
    display_tab2( it_tab2 = lt_tab2 out = out ).



    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).

    MOVE-CORRESPONDING lt_tab1 TO lt_tab2 EXPANDING NESTED TABLES.
    out->write( '>>MOVE-CORRESPONDING lt_tab1 to lt_tab2 EXPANDING NESTED TABLES.' ).
    display_tab2( it_tab2 = lt_tab2 out = out ).


    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).

    MOVE-CORRESPONDING lt_tab1 TO lt_tab2 KEEPING TARGET LINES.
    out->write( '>>MOVE-CORRESPONDING lt_tab1 to lt_tab2 KEEPING TARGET LINES.' ).
    display_tab2( it_tab2 = lt_tab2 out = out ).



    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).

    MOVE-CORRESPONDING lt_tab1 TO lt_tab2 EXPANDING NESTED TABLES KEEPING TARGET LINES.
    out->write( '>>MOVE-CORRESPONDING lt_tab1 to lt_tab2 EXPANDING NESTED TABLES KEEPING TARGET LINES.' ).
    display_tab2( it_tab2 = lt_tab2 out = out ).
  ENDMETHOD.


  METHOD corresponding_operator.
    "https://www.youtube.com/watch?v=4KA_s7ct1Pw
    "compare with the operations in move_corresponding.

    "To Do:
    "1. MAPPING in Corresponding.
    "2. EXCEPT  in Corresponding.
    "3. RAP specific Corresponding.
    DATA:
      ls_struct1 TYPE ty_struct1,
      ls_struct2 TYPE ty_struct2.


    clear_fill_structures(
      CHANGING
        cs_struct2 = ls_struct2
        cs_struct1 = ls_struct1 ).

    me->display_structure1( out = out is_struct1 = ls_struct1 ).
    me->display_structure2( out = out is_struct2 = ls_struct2 ).
**********************************************************************
    new_line( out ).
    out->write( '>> ls_struct2 = CORRESPONDING #( ls_struct1 ).' ).
    new_line( out ).
    out->write( |Note: It's not same as MOVE-CORRESPONDING notice below COL4 for difference \n    as it's target stucture is first overridden| ).

    ls_struct2 = CORRESPONDING #( ls_struct1 ).
    me->display_structure2( out = out is_struct2 = ls_struct2 ).
**********************************************************************
    new_line( out ).
    new_line( out ).
    clear_fill_structures( CHANGING cs_struct2 = ls_struct2 cs_struct1 = ls_struct1 ).
    out->write( '>> ls_struct2 = CORRESPONDING #( BASE ( ls_struct2 ) ls_struct1 ).' ).
    out->write( |Note: It's same as MOVE-CORRESPONDING| ).

    ls_struct2 = CORRESPONDING #( BASE ( ls_struct2 ) ls_struct1 ).
    me->display_structure2( out = out is_struct2 = ls_struct2 ).
**********************************************************************
    new_line( out ).
    clear_fill_structures( CHANGING cs_struct2 = ls_struct2 cs_struct1 = ls_struct1 ).
    out->write( 'MOVE-CORRESPONDING ls_struct1 to ls_struct2 EXPANDING NESTED TABLES.' ).

    MOVE-CORRESPONDING ls_struct1 TO ls_struct2 EXPANDING NESTED TABLES.
    me->display_structure2( out = out is_struct2 = ls_struct2 ).
**********************************************************************
    new_line( out ).
    clear_fill_structures( CHANGING cs_struct2 = ls_struct2 cs_struct1 = ls_struct1 ).
    out->write( 'ls_struct2 = CORRESPONDING #( DEEP BASE ( ls_struct2 ) ls_struct1 ).' ).

    ls_struct2 = CORRESPONDING #( DEEP BASE ( ls_struct2 ) ls_struct1 ).
    me->display_structure2( out = out is_struct2 = ls_struct2 ).
**********************************************************************
    DATA: lt_tab1 TYPE  tt_tab1,
          lt_tab2 TYPE tt_tab2.

    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).

    out->write( '===============Begin TABLE section===============' ).
    display_tab1( it_tab1 = lt_tab1 out = out ).
    display_tab2( it_tab2 = lt_tab2 out = out ).
**********************************************************************
    new_line( out ).
    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).
    out->write( '>>lt_tab2 = CORRESPONDING #( lt_tab1 ).' ).
    lt_tab2 = CORRESPONDING #( lt_tab1 ).
    display_tab2( it_tab2 = lt_tab2 out = out ).

**********************************************************************
    new_line( out ).
    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).
    out->write( '>>MOVE-CORRESPONDING lt_tab1 to lt_tab2.' ).
    MOVE-CORRESPONDING lt_tab1 TO lt_tab2.
    display_tab2( it_tab2 = lt_tab2 out = out ).
**********************************************************************
    new_line( out ).
    new_line( out ).
    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).
    lt_tab2 = CORRESPONDING #( DEEP lt_tab1 ).
    out->write( '>>lt_tab2 = CORRESPONDING #( DEEP lt_tab1 ).' ).
    display_tab2( it_tab2 = lt_tab2 out = out ).
**********************************************************************
    new_line( out ).
    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).
    MOVE-CORRESPONDING lt_tab1 TO lt_tab2 EXPANDING NESTED TABLES.
    out->write( '>>MOVE-CORRESPONDING lt_tab1 to lt_tab2 EXPANDING NESTED TABLES.' ).
    display_tab2( it_tab2 = lt_tab2 out = out ).
**********************************************************************
    new_line( out ).
    new_line( out ).
    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).
    out->write( '>>lt_tab2 = CORRESPONDING #( BASE ( lt_tab2 ) lt_tab1 ).' ).
    lt_tab2 = CORRESPONDING #( BASE ( lt_tab2 ) lt_tab1 ).
    display_tab2( it_tab2 = lt_tab2 out = out ).

**********************************************************************
    new_line( out ).
    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).

    MOVE-CORRESPONDING lt_tab1 TO lt_tab2 KEEPING TARGET LINES.
    out->write( '>>MOVE-CORRESPONDING lt_tab1 to lt_tab2 KEEPING TARGET LINES.' ).
    display_tab2( it_tab2 = lt_tab2 out = out ).
**********************************************************************
    new_line( out ).
    new_line( out ).
    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).
    out->write( '>>lt_tab2 = CORRESPONDING #( DEEP BASE ( lt_tab2 ) lt_tab1 ).' ).
    lt_tab2 = CORRESPONDING #( DEEP BASE ( lt_tab2 ) lt_tab1 ).
    display_tab2( it_tab2 = lt_tab2 out = out ).

**********************************************************************
    new_line( out ).
    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).

    MOVE-CORRESPONDING lt_tab1 TO lt_tab2 EXPANDING NESTED TABLES KEEPING TARGET LINES.
    out->write( '>>MOVE-CORRESPONDING lt_tab1 to lt_tab2 EXPANDING NESTED TABLES KEEPING TARGET LINES.' ).
    display_tab2( it_tab2 = lt_tab2 out = out ).
*********************************************************************

  ENDMETHOD.


  METHOD clear_fill_itabs.
    CLEAR: ct_tab1, ct_tab2.
    ct_tab1 = VALUE #(
              (
                col1 = 'a1'
                col2 = 'a2'
                tab = VALUE #( ( col1 = 'a11' col2 = 'a12' )
                               ( col1 = 'a21' col2 = 'a22' )
                             )
              )
              (
                col1 = 'b1'
                col2 = 'b2'
                tab = VALUE #( ( col1 = 'b11' col2 = 'b12' )
                               ( col1 = 'b21' col2 = 'b22' )
                             )
              )
).


    ct_tab2 = VALUE #(
                        (
                              col2 = 'x1'
                              tab = VALUE #(
                                              ( col2 = 'x11' col3 = 'x12' )
                                              ( col2 = 'x21' col3 = 'x22' )
                                              ( col2 = 'x31' col3 = 'x32' )
                                      )
                               col4 = 'x4'
                         )
                         (
                              col2 = 'y1'
                              tab = VALUE #(
                                              ( col2 = 'y11' col3 = 'y12' )
                                              ( col2 = 'y21' col3 = 'y22' )
                                              ( col2 = 'y31' col3 = 'y32' )
                                      )
                               col4 = 'y4'
                         )


    ).

  ENDMETHOD.


  METHOD clear_fill_structures.
    CLEAR: cs_struct1,cs_struct2.

    cs_struct1 = VALUE #(
                      col1 = 'a1'
                      col2 = 'a2'
                      tab = VALUE #( ( col1 = 'a11' col2 = 'a12' )
                                     ( col1 = 'a21' col2 = 'a22' )
                                   )
    ).


    cs_struct2 = VALUE #(
                              col2 = 'x1'
                              tab = VALUE #(
                                              ( col2 = 'x11' col3 = 'x12' )
                                              ( col2 = 'x21' col3 = 'x22' )
                                              ( col2 = 'x31' col3 = 'x32' )
                                      )
                               col4 = 'x4'
    ).

  ENDMETHOD.


  METHOD embedded_expressions.

    "Embedded Expressions
    "-https://www.youtube.com/watch?v=DMLvh27e2ZU&t=26

    "Literals(unnamed data obj with fixed values).
    "Text Literals = '' max255 and trailing space ignored
    "String Literals = `` max255 and trailing space not ignored

    CONSTANTS lc_val TYPE char255 VALUE 'Akp  '.

    out->write( strlen( 'Akp  ' ) ).  "Text Literals
    out->write( strlen( `Akp  ` ) ).  "String Literals

    DATA: lv_matnr TYPE matnr VALUE '000000000123'.
    out->write( 'Alpha Conversions:' ).
    out->write( |{ lv_matnr }| ).
    out->write( |{ lv_matnr ALPHA = OUT }| ).
    new_line( out ).

    out->write( 'DATE/TIME/NUM conversions:' ).
    out->write( |{ sy-datum }| ).
    out->write( |{ sy-datum DATE = USER }| ).

    out->write( |{ sy-uzeit }| ).
    out->write( |{ sy-uzeit TIME = USER }| ).
    new_line( out ).

    DATA(lv_num) = VALUE int4( ).
    lv_num = 13534.
    out->write( |{ lv_num }| ).
    out->write( |{ lv_num NUMBER = USER }| ).

    out->write( |string operations:| ).
    out->write( |{ lc_val }\n{ lc_val CASE = UPPER }| ).  "\n - control character, in the same cell new line added
    "\t - control character adds a tab

  ENDMETHOD.

  METHOD new_line.

    i_out->write( |\n| ).

  ENDMETHOD.




  METHOD new_vs_value.

    "NEW Vs VALUE
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "NEW will always return a reference variable.

    TYPES: tt_akp_emp TYPE TABLE OF zdt_akp_emp WITH DEFAULT KEY.
    TYPES: ts_akp_emp TYPE zdt_akp_emp.


    DATA(ls_akp_emp) = NEW ts_akp_emp(  client = sy-mandt id = 1 name = 'ashish' age = 32 ).

    out->write( data = ls_akp_emp->* name = 'derefenced structure' ).  "
    out->write( data = ls_akp_emp->name ).  "


    DATA(lt_akp_emp) = NEW tt_akp_emp( ( client = sy-mandt id = 1 name = 'ashish' age = 32 )
                                       ( client = sy-mandt id = 2 name = 'andrew' age = 22 ) ).

    out->write( data = lt_akp_emp->* name = 'derefenced table' ).  "
    out->write( data = lt_akp_emp->*[ 2 ]-name ).  "

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "VALUE will return the actual variables.

    DATA(ls_akp_emp1) = VALUE ts_akp_emp( client = sy-mandt id = 1 name = 'ashish' age = 32 ).

    out->write( data = ls_akp_emp1 name = 'value structure' ).  "
    out->write( data = ls_akp_emp->name ).  "


    DATA(lt_akp_emp1) = VALUE tt_akp_emp( ( client = sy-mandt id = 1 name = 'ashish' age = 32 )
                                       ( client = sy-mandt id = 2 name = 'andrew' age = 22 ) ).

    out->write( data = lt_akp_emp1 name = 'value table' ).  "
    out->write( data = lt_akp_emp1[ 2 ]-name ).  "

*    appending and assigning to a new table
    DATA(lt_akp_emp_appended) = VALUE #( BASE lt_akp_emp1 ( client = sy-mandt id = 3 name = 'charlie' age = 25 )  ).
    out->write( data = lt_akp_emp_appended name = 'appended table:' ).

  ENDMETHOD.


  METHOD fieldSymbol_vs_dataReference.

    "https://www.youtube.com/watch?v=T39CPoESgqQ
    "Field symbol vs Data reference variable.
    "->Data reference variable : A variable that holds a reference to a memory location of another variable.
    "->Field Symbol            : A variable that holds a reference to a field's value.

    DATA: lv_ref_int1 TYPE REF TO i.

    CREATE DATA lv_ref_int1.
    lv_ref_int1->* = 1.          "Dereference operator to access the variable/data object's value stored in it's memory location.

    ASSIGN lv_ref_int1->* TO FIELD-SYMBOL(<lfs_int>).
    out->write( lv_ref_int1->* ).
    out->write( <lfs_int> ).

    "Alternate way of declaring Data reference variable using NEW constructor syntax.
    DATA(lv_ref_int2) = NEW i( 2 ).
    out->write( lv_ref_int2->* ).

  ENDMETHOD.


  METHOD inline_declaration.

*    Inline declaration.   --https://www.youtube.com/watch?v=AFPVxzghIUs

    SELECT FROM /dmo/travel_m
    FIELDS travel_id,agency_id
    INTO TABLE @DATA(lt_travel)
    UP TO 5 ROWS.
    IF sy-subrc = 0.
      DATA(travel_lines) = lines( lt_travel ).
    ENDIF.

    LOOP AT lt_travel ASSIGNING FIELD-SYMBOL(<lfs_travel>).

    ENDLOOP.

    READ TABLE lt_travel ASSIGNING FIELD-SYMBOL(<lfs_travel1>) INDEX 1.

    out->write( lt_travel ).

  ENDMETHOD.


  METHOD display_structure1.
    TYPES:
      BEGIN OF ts_dispaly_table1,
        col1     TYPE char3,
        col2     TYPE char3,
        tab_col1 TYPE char3,
        tab_col2 TYPE char3,
      END OF ts_dispaly_table1.
    DATA ls_display_table1 TYPE ts_dispaly_table1.
    DATA lt_struct1 TYPE STANDARD TABLE OF ts_dispaly_table1 WITH EMPTY KEY.

    ls_display_table1-col1 = is_struct1-col1.
    ls_display_table1-col2 = is_struct1-col2.
    LOOP AT is_struct1-tab ASSIGNING FIELD-SYMBOL(<lfs_tab>).
      ls_display_table1-tab_col1 =  <lfs_tab>-col1.
      ls_display_table1-tab_col2 = <lfs_tab>-col2.
      IF sy-tabix > 1.
        CLEAR ls_display_table1-col1.
        CLEAR ls_display_table1-col2.
      ENDIF.
      APPEND ls_display_table1 TO lt_struct1.

    ENDLOOP.


    out->write( 'structure 1:' ).

    out->write( lt_struct1 ).


  ENDMETHOD.


  METHOD display_tab1.
    TYPES:
      BEGIN OF ts_dispaly_table1,
        col1     TYPE char3,
        col2     TYPE char3,
        tab_col1 TYPE char3,
        tab_col2 TYPE char3,
      END OF ts_dispaly_table1.
    DATA ls_display_table1 TYPE ts_dispaly_table1.
    DATA lt_struct1 TYPE STANDARD TABLE OF ts_dispaly_table1 WITH EMPTY KEY.

    LOOP AT it_tab1 ASSIGNING FIELD-SYMBOL(<lfs_tab1>).
      ls_display_table1-col1 = <lfs_tab1>-col1.
      ls_display_table1-col2 = <lfs_tab1>-col2.
      LOOP AT <lfs_tab1>-tab ASSIGNING FIELD-SYMBOL(<lfs_tab>).
        ls_display_table1-tab_col1 =  <lfs_tab>-col1.
        ls_display_table1-tab_col2 = <lfs_tab>-col2.
        IF sy-tabix > 1.
          CLEAR ls_display_table1-col1.
          CLEAR ls_display_table1-col2.
        ENDIF.
        APPEND ls_display_table1 TO lt_struct1.

      ENDLOOP.
    ENDLOOP.

    out->write( 'tab 1:' ).

    out->write( lt_struct1 ).
  ENDMETHOD.


  METHOD display_structure2.
    TYPES:
      BEGIN OF ts_dispaly_table2,
        col2     TYPE char3,
        tab_col2 TYPE char3,
        tab_col3 TYPE char3,
        col4     TYPE char3,
      END OF ts_dispaly_table2.
    DATA ls_display_table2 TYPE ts_dispaly_table2.
    DATA lt_struct2 TYPE STANDARD TABLE OF ts_dispaly_table2 WITH EMPTY KEY.

    ls_display_table2-col2 = is_struct2-col2.
    ls_display_table2-col4 = is_struct2-col4.
    LOOP AT is_struct2-tab ASSIGNING FIELD-SYMBOL(<lfs_tab>).
      ls_display_table2-tab_col2 =  <lfs_tab>-col2.
      ls_display_table2-tab_col3 = <lfs_tab>-col3.
      IF sy-tabix > 1.
        CLEAR ls_display_table2-col2.
        CLEAR ls_display_table2-col4.
      ENDIF.
      APPEND ls_display_table2 TO lt_struct2.

    ENDLOOP.

    out->write( 'structure 2:' ).

    out->write( lt_struct2 ).

  ENDMETHOD.


  METHOD display_tab2.
    TYPES:
      BEGIN OF ts_dispaly_table2,
        col2     TYPE char3,
        tab_col2 TYPE char3,
        tab_col3 TYPE char3,
        col4     TYPE char3,
      END OF ts_dispaly_table2.
    DATA ls_display_table2 TYPE ts_dispaly_table2.
    DATA lt_struct2 TYPE STANDARD TABLE OF ts_dispaly_table2 WITH EMPTY KEY.

    LOOP AT it_tab2 ASSIGNING FIELD-SYMBOL(<lfs_tab2>).
      ls_display_table2-col2 = <lfs_tab2>-col2.
      ls_display_table2-col4 = <lfs_tab2>-col4.
      LOOP AT <lfs_tab2>-tab ASSIGNING FIELD-SYMBOL(<lfs_tab>).
        ls_display_table2-tab_col2 =  <lfs_tab>-col2.
        ls_display_table2-tab_col3 = <lfs_tab>-col3.
        IF sy-tabix > 1.
          CLEAR ls_display_table2-col2.
          CLEAR ls_display_table2-col4.
        ENDIF.
        APPEND ls_display_table2 TO lt_struct2.

      ENDLOOP.

    ENDLOOP.

    out->write( 'tab 2:' ).

    out->write( lt_struct2 ).
  ENDMETHOD.


  METHOD let_expression.
    "https://www.youtube.com/watch?v=YfK-2_a19bI
    SELECT SINGLE FROM /dmo/travel_m
    FIELDS *
    WHERE travel_id = '00000002'
    INTO @DATA(ls_travel).
**********************************************************************
    DATA(lv_tot_price_and_disc_price) = CONV string(
      LET
    disc                 = 10
    sep                  = ' , '
    price_after_discount = ls_travel-total_price - disc
                           IN |{ ls_travel-total_price }{ sep }{ price_after_discount }|
  ).
    out->write( lv_tot_price_and_disc_price ).
**********************************************************************

    DATA(lv_tot_price_and_disc_price1) = CONV string(
      LET
    disc                 = 20
    sep                  = ' - '
    price_after_discount = ls_travel-total_price - disc
                           IN |{ ls_travel-total_price }{ sep }{ price_after_discount }|
  ).

    out->write( lv_tot_price_and_disc_price1 ).
**********************************************************************
    TYPES:
      BEGIN OF date,
        year  TYPE c LENGTH 4,
        month TYPE c LENGTH 2,
        day   TYPE c LENGTH 2,
      END OF date,
      dates TYPE TABLE OF date WITH EMPTY KEY.

    FINAL(dates) = VALUE dates(
      ( year = '2013' month = '07' day = '16' )
      ( year = '2014' month = '08' day = '31' )
      ( year = '2015' month = '09' day = '07' ) ).

    DO lines( dates ) TIMES.
      FINAL(isodate) = CONV string(
        LET
      <date>    = dates[ sy-index ]
      separator = '-'
                  IN <date>-year && separator && <date>-month && separator && <date>-day ).
      out->write( isodate ).
    ENDDO.
**********************************************************************
    out->write( '----' ).
    TYPES:
      BEGIN OF struc,
        col1 TYPE i,
        col2 TYPE i,
      END OF struc.

    FINAL(t) = sy-uzeit.
    FINAL(rnd) = cl_abap_random_int=>create(
      seed = CONV i( t ) min = 1 max = 10 ).

    DO 5 TIMES.
      FINAL(struc) = VALUE struc(
        LET x = rnd->get_next( )
            y = x * x
            z = sy-index * 1000 IN col1 = x + z
                                   col2 = y + z ).
      out->write( struc ).
      new_line( out ).
    ENDDO.

**********************************************************************



  ENDMETHOD.


  METHOD for_loop.
    "https://www.youtube.com/watch?v=u6fQBP57CpU&t=68
*0.  Let expression in ABAP
*1.  For:-Get some of the columns from one internal table to new internal table
*2.  For:-Get some of lines based on some condition from one table to another table
*3.  For:-Changing sequence of internal table some time needed to sort:-
*4.  For:-Preparing a range table
*5.  For:-Getting some data from third party want to add date field additional before updating in db tabe
*6.  For:-C like for loop
    TYPES: BEGIN OF lty_pricedetail,
             travel_id     TYPE /dmo/travel_id,
             agency_id     TYPE /dmo/agency_id,
             total_price   TYPE /dmo/total_price,
             currency_code TYPE /dmo/currency_code,
           END OF lty_pricedetail.
    DATA: lt_travel_pricedetails TYPE STANDARD TABLE OF lty_pricedetail.

    SELECT  FROM /dmo/travel_m
    FIELDS *
    INTO TABLE @DATA(lt_travel)
    UP TO 5 ROWS.

    CHECK sy-subrc EQ 0.
**********************************************************************
*1.  For:-Get some of the columns from one internal table to new internal table
    out->write( |*1. For:-Get some of the columns from one internal table to new internal table| ).
    lt_travel_pricedetails = VALUE #( FOR ls_travel IN lt_travel
                                                                  (
                                                                   travel_id = ls_travel-travel_id
                                                                   agency_id = ls_travel-agency_id
                                                                   total_price = ls_travel-total_price
                                                                   currency_code = ls_travel-currency_code
                                                                    ) ).
    out->write( lt_travel_pricedetails ).
**********************************************************************
*   Alternate syntax for above case 1
    out->write( |> Alternate syntax for above case 1| ).
    CLEAR: lt_travel_pricedetails.
    lt_travel_pricedetails = VALUE #( FOR ls_travel IN lt_travel (
                                                            CORRESPONDING #( ls_travel )
                                                                 )
                                    ).
    out->write( lt_travel_pricedetails ).

**********************************************************************
*2.  For:-Get some of lines based on some condition from one table to another table
    new_line( out ).
    out->write( |*2.  For:-Get some of lines based on some condition from one table to another table| ).
    CLEAR: lt_travel_pricedetails.
    lt_travel_pricedetails = VALUE #( FOR ls_travel IN lt_travel WHERE ( total_price < 1000 )
                                                                  (
                                                                    CORRESPONDING #( ls_travel )
                                                                    ) ).
    out->write( lt_travel_pricedetails ).
**********************************************************************
*4.  For:-Preparing a range table
    new_line( out ).
    out->write( |*4.  For:-Preparing a range table| ).
    DATA lt_range_agency TYPE RANGE OF /dmo/agency_id.

    lt_range_agency = VALUE #( FOR ls_travel IN lt_travel
                                            (
                                                sign = 'I'
                                                option = 'EQ'
                                                low = ls_travel-agency_id
                                            )
                             ).
    out->write( lt_range_agency ).
**********************************************************************
*5.1 Add few fields manually and rest using CORRESPONDING
    new_line( out ).
    out->write( |*5.1 Add few fields manually and rest using CORRESPONDING| ).
    TYPES: BEGIN OF lty_pricedetail_expanded,
             travel_id     TYPE /dmo/travel_id,
             agency_id     TYPE /dmo/agency_id,
             total_price   TYPE /dmo/total_price,
             currency_code TYPE /dmo/currency_code,
             zcurrentdate  TYPE sy-datum,
             zusername     TYPE sy-uname,
           END OF lty_pricedetail_expanded.

    DATA lt_expanded_pricedetails TYPE STANDARD TABLE OF lty_pricedetail_expanded.

    lt_expanded_pricedetails = VALUE #( FOR ls_travel IN lt_travel
                                                 LET ls_base = VALUE lty_pricedetail_expanded(
                                                                                 zcurrentdate = sy-datum
                                                                                 zusername = sy-uname
*                                                                                 currency_code = 'INR'  This case is not working need to check
                                                                                             )
                                                 IN
                                                        (
                                                            CORRESPONDING #( BASE ( ls_base ) ls_travel )
                                                        )

                                      ).

    out->write( lt_expanded_pricedetails ).
**********************************************************************
*5.2 Add few fields manually and rest using CORRESPONDING -Fixes above issue *Important
    new_line( out ).
    out->write( |*Fixes above issue *Important| ).
    out->write( |*5.2 Add few fields manually and rest using CORRESPONDING | ).
    lt_expanded_pricedetails = VALUE #( FOR ls_travel IN lt_travel
                                            LET ls_base = VALUE lty_pricedetail_expanded(
                                                                                         zcurrentdate = sy-datum
                                                                                         zusername = sy-uname
                                                                                         currency_code = 'INR'
                                                                                        )
                                            IN (
                                                   CORRESPONDING #( BASE ( ls_base ) ls_travel EXCEPT currency_code )
                                                   "not needed to put z field in exception list as they are not present in lt_travel
                                                )
                                        ).
    out->write( lt_expanded_pricedetails ).
**********************************************************************
*6.  For:-C like for loop
    new_line( out ).
    out->write( |*6.  For:-C like for loop| ).
    TYPES: BEGIN OF ty_numbers,
             number TYPE i,
             square TYPE i,
             cube   TYPE i,
           END OF ty_numbers,
           ty_t_numbers TYPE STANDARD TABLE OF ty_numbers WITH DEFAULT KEY.

    DATA(lt_numbers) = VALUE ty_t_numbers( FOR i = 1 THEN i + 1 WHILE i LE 10
                                            ( number = i
                                              square = i * i
                                              cube = i * i * i
                                             )
                                         ).
    out->write( lt_numbers ).
**********************************************************************

  ENDMETHOD.
  METHOD reduce_operator.
    "https://www.youtube.com/watch?v=LMFpX3LNdgU&t=19s

    TYPES: BEGIN OF ty_final,
             carrier_id          TYPE /dmo/carrier-carrier_id,
             name                TYPE /dmo/carrier-name,
             total_price         TYPE /dmo/flight-price,
             flight_date_options TYPE string,
           END OF ty_final.
    TYPES: BEGIN OF ty_calc_fields,
             total_price         TYPE /dmo/flight-price,
             flight_date_options TYPE string,
           END OF ty_calc_fields.
    DATA: lt_final TYPE STANDARD TABLE OF ty_final.

    SELECT FROM /dmo/carrier
    FIELDS carrier_id , name
    INTO TABLE @DATA(lt_carriers).

    CHECK sy-subrc EQ 0.

    SELECT FROM /dmo/flight
    FIELDS carrier_id, connection_id, flight_date, price
    FOR ALL ENTRIES IN @lt_carriers
    WHERE carrier_id = @lt_carriers-carrier_id
    INTO TABLE @DATA(lt_flights).

    CHECK sy-subrc EQ 0.

    "Prepare a final table combining data from carrier and flights data.
    "show all flight dates for carries separated by '/'
    "show total of all flights prices for each carrier.
    LOOP AT lt_carriers ASSIGNING FIELD-SYMBOL(<ls_carrier>).
      APPEND INITIAL LINE TO lt_final ASSIGNING FIELD-SYMBOL(<ls_final>).

      <ls_final>-carrier_id = <ls_carrier>-carrier_id.
      <ls_final>-name = <ls_carrier>-name.


*      <ls_final>-total_price = REDUCE #( INIT tot_price = VALUE /dmo/flight_price(  )
*                                            FOR ls_flight IN lt_flights WHERE ( carrier_id = <ls_carrier>-carrier_id )
*                                            NEXT tot_price = tot_price + ls_flight-price
*                                        ).
*
*      <ls_final>-flight_date_options = REDUCE #( INIT lv_all_flight_dates TYPE string
*                                                      sep = ''
*                                                   FOR ls_flight IN lt_flights WHERE ( carrier_id = <ls_carrier>-carrier_id )
*                                                  NEXT lv_all_flight_dates = |{ lv_all_flight_dates }{ sep }{ ls_flight-flight_date  }|
*                                                       sep = '/ '
*                                                ).

      "We can also calculate both of the above two fields together in 1 iteration with a helper type.
      DATA(ls_calc_fileds) = REDUCE ty_calc_fields( INIT wa_calc_fields TYPE ty_calc_fields
                                                         sep = ''
                                                   FOR ls_flight IN lt_flights WHERE ( carrier_id = <ls_carrier>-carrier_id )

                                                   NEXT wa_calc_fields-total_price = wa_calc_fields-total_price + ls_flight-price
                                                        wa_calc_fields-flight_date_options = |{ wa_calc_fields-flight_date_options }{ sep }{ ls_flight-flight_date  }|
                                                        sep = '/ '

                                                    ).

      <ls_final>-total_price = ls_calc_fileds-total_price.
      <ls_final>-flight_date_options = ls_calc_fileds-flight_date_options.

      CLEAR ls_calc_fileds.

    ENDLOOP.

    out->write( lt_final ).

    "IMP Points for REDUCE operator

* The variables or field symbols declared after INIT can only be used after NEXT.
* INIT is mandatory, it creates local variable, and at least one variable is mandatory
* The first variable after INIT determines the result of the REDUCE expression and the data type must be convertible to the result type
* All other variables declared after the first variable of INIT are optional helper fields which can also be modified in the NEXT section.
* We can also use LET in REDUCE, however these variable can't be modified in the NEXT section, only can be used in NEXT section.
* At least one iteration expression must then be specified using FOR conditional iterations on table iterations
* NEXT that are executed for every iteration of the last FOR expression


    "We can also use LET in REDUCE as below, however these variable can't be modified in NEXT
*      <ls_final>-flight_date_options = REDUCE #(   LET sep = '*' IN
*                                                   INIT lv_all_flight_dates TYPE string
*                                                   FOR ls_flight IN lt_flights WHERE ( carrier_id = <ls_carrier>-carrier_id )
*                                                  NEXT lv_all_flight_dates = |{ ls_flight-flight_date }{ sep }{ lv_all_flight_dates }|
*
*                                                ).


***********************************************************************************
*    2nd example
    new_line( out ).
    out->write( 'Concatenation without THEN , default taken as n+1' ).

    out->write( REDUCE #( INIT text = `Count Up:`
                          FOR n = 1 UNTIL n > 10    "helps to read as UNTIL AND UNLESS
                          NEXT text = |{ text } { n }|
                        ) ).

    new_line( out ).
    out->write( 'Concatenation with THEN' ).

    out->write( REDUCE #( INIT text = `Count Down:`
                          FOR n = 10 THEN n - 1 WHILE n >= 0
                          NEXT text = |{ text } { n }|
                ) ).

    new_line( out ).
    out->write( 'Non arithmetic expression' ).

    out->write( REDUCE #( INIT text = ``
                          FOR t = `x` THEN |{ t }y| WHILE strlen( t ) <= 5
                          NEXT text = |{ text }{ t } |

    ) ).




  ENDMETHOD.

  METHOD group_by.
    TYPES: BEGIN OF ty_flight,
             carrier_id    TYPE /dmo/flight-carrier_id,
             connection_id TYPE /dmo/flight-connection_id,
             flight_date   TYPE /dmo/flight-flight_date,
           END OF ty_flight.
    TYPES: BEGIN OF ty_flight_1,
             carrier_id    TYPE /dmo/flight-carrier_id,
             connection_id TYPE /dmo/flight-connection_id,
             flight_date   TYPE /dmo/flight-flight_date,
             count         TYPE i,
             index         TYPE i,
           END OF ty_flight_1.
    DATA: lt_flights     TYPE STANDARD TABLE OF ty_flight,
          lt_flights_grp TYPE STANDARD TABLE OF ty_flight.

    "https://www.youtube.com/watch?v=95a5YsLnhvs&t=44s

    SELECT
    FROM /dmo/flight
    FIELDS carrier_id, connection_id, flight_date
    INTO TABLE @lt_flights
    UP TO 9 ROWS.

***********************************************************************************************
    out->write( |======== Group By - Representative Binding ========| ).
***********************************************************************************************
    new_line( out ).
    out->write( '>This field symbol represents the whole group of records,').
    out->write( '>however here it will just print the first row of each group.' ).
    new_line( out ).

    LOOP AT lt_flights ASSIGNING FIELD-SYMBOL(<lfs_flight>) "Representative Binding
                                    GROUP BY ( carrier_id = <lfs_flight>-carrier_id
                                               connection_id = <lfs_flight>-connection_id ).

      out->write( <lfs_flight> ).  "This field symbol represents the whole group of records, however here it will just print the first row of each group.

    ENDLOOP.

***********************************************************************************************
    new_line( out ).
    out->write( '>Here using LOOP AT GROUP printing each line of group' ).
    new_line( out ).

    LOOP AT lt_flights ASSIGNING FIELD-SYMBOL(<lfs_flight1>) "Representative Binding
                                    GROUP BY ( carrier_id = <lfs_flight1>-carrier_id
                                               connection_id = <lfs_flight1>-connection_id ).
      CLEAR lt_flights_grp.
      LOOP AT GROUP <lfs_flight1> ASSIGNING FIELD-SYMBOL(<lfs_grp_entry>).

        lt_flights_grp = VALUE #( BASE lt_flights_grp ( <lfs_grp_entry> ) ).

      ENDLOOP.

      out->write( lt_flights_grp ).

    ENDLOOP.
***********************************************************************************************
    "https://www.youtube.com/watch?v=2oQcaPHP5MU
    new_line( out ).
    out->write( |======== Group By - Group key Binding ========| ).
***********************************************************************************************


    LOOP AT lt_flights ASSIGNING FIELD-SYMBOL(<lfs_flight2>)
                                    GROUP BY ( carrier_id = <lfs_flight2>-carrier_id
                                               connection_id = <lfs_flight2>-connection_id )
                                        ASSIGNING FIELD-SYMBOL(<lt_grp>). "Group Key binding
      CLEAR lt_flights_grp.

*      LOOP AT GROUP <lt_grp> ASSIGNING FIELD-SYMBOL(<lfs_grp_entry1>).
*        lt_flights_grp = VALUE #( BASE lt_flights_grp ( <lfs_grp_entry1> ) ).
*      ENDLOOP.

      "Above LOOP AT GROUP can be replaced by below , FOR..IN GROUP
      lt_flights_grp = VALUE #( FOR <grp_entry> IN GROUP <lt_grp> ( CORRESPONDING #( <grp_entry> ) ) ).

      out->write( lt_flights_grp ).

    ENDLOOP.
***********************************************************************************************
    new_line( out ).
    out->write( |>Variations in Group Key binding| ).
    new_line( out ).

    DATA lt_flights_1 TYPE STANDARD TABLE OF ty_flight_1.
    lt_flights_1 = CORRESPONDING #( lt_flights ).

    LOOP AT lt_flights_1 ASSIGNING FIELD-SYMBOL(<lfs_flight3>)
                                    GROUP BY ( carrier_id = <lfs_flight3>-carrier_id
                                               connection_id = <lfs_flight3>-connection_id
                                               count = GROUP SIZE
                                               index = GROUP INDEX )
                                               DESCENDING
*                                        WITHOUT MEMBERS " we can use only the group keys in <lt_grp>, hence it will be faster.
                                        ASSIGNING FIELD-SYMBOL(<lt_grp1>). "Group Key binding
      out->write( <lt_grp1> ).

    ENDLOOP.
***********************************************************************************************
    "https://www.youtube.com/watch?v=crHfXsHhwp0
    new_line( out ).
    out->write( |Loop at GROUP BY - Real time example| ).
    "Typical use is insteady of AT END , AT BEGIN  we can use LOOP AT GROUP BY.
    "for example for calulation of item total price ,
***********************************************************************************************
    TYPES zbapiret2 TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY.
    DATA(lt_return) = VALUE Zbapiret2(
                                ( type = 'A' message = 'message type A' )
                                ( type = 'W' message = 'message type W' )
                                ( type = 'I' message = 'message type I' )
                                ( type = 'E' message = 'message type E' )
                                ( type = 'S' message = 'message type S' )
                                ( type = 'X' message = 'message type X' )

        ).

    LOOP AT lt_return INTO DATA(ls_return)
            GROUP BY translate( val = ls_return-type from = 'AXEIWS' to = '012345')
            ASCENDING.

      DATA(lv_message) = ls_return-message.
      EXIT.
    ENDLOOP.
    "This can also be done with for with where. interesting to check.
    out->write( lv_message  ).



  ENDMETHOD.

  METHOD filter_operator.
    "https://www.youtube.com/watch?v=7ji98-IFPQc&t=194s
*Below are some points to keep in mind when using the FILTER operator

*-> The internal table on which FILTER operator is used must have at least one sorted key or one hash key used for access.

*->The row type of main internal table and result internal table do NOT have to be identical.

*->The Boolean operators NOT, OR, and EQUIV cannot be used in the WHERE condition.

*->Table - filtering can also be performed using a table comprehension or a table reduction
*  with an iteration expression for table iterations with FOR.
*  The operator FILTER provides a shortened format for this special case and is more efficient to execute.

*->A table filter constructs the result row by row. If the result contains almost all row in the source table,
*  this method can be slower than copying the source table and deleting the surplus rows from the target table.
    TYPES: BEGIN OF ty_flight,
             carrier_id    TYPE /dmo/flight-carrier_id,
             connection_id TYPE /dmo/flight-connection_id,
             flight_date   TYPE /dmo/flight-flight_date,
           END OF ty_flight,
           ty_t_flight TYPE STANDARD TABLE OF ty_flight WITH DEFAULT KEY,
           BEGIN OF ty_carrier,
             carrier_id    TYPE /dmo/carrier-carrier_id,
             currency_code TYPE /dmo/carrier-currency_code,
           END OF ty_carrier.
*    DATA: lt_usd_carriers TYPE STANDARD TABLE OF ty_carrier.
    DATA: lt_usd_carriers TYPE SORTED TABLE OF ty_carrier WITH UNIQUE KEY carrier_id.

    DATA: lt_flights        TYPE STANDARD TABLE OF ty_flight WITH NON-UNIQUE SORTED KEY car COMPONENTS carrier_id,
          lt_flights_sorted TYPE SORTED TABLE OF ty_flight WITH NON-UNIQUE KEY carrier_id.

    SELECT
    FROM /dmo/flight
    FIELDS carrier_id, connection_id, flight_date
    WHERE carrier_id IN ( 'SQ', 'AA', 'AZ' )
    INTO TABLE @lt_flights.

    CHECK sy-subrc = 0.
    out->write( |Original data| ).
    out->write( lt_flights ).
**************************************************************************************************************
    new_line( out ).
    out->write( |Filterd Data using Key| ).
    DATA(lt_flights_az) = FILTER ty_t_flight( lt_flights USING KEY car WHERE carrier_id = CONV #( 'AZ' )  ).
    out->write( lt_flights_az ).

**************************************************************************************************************
    lt_flights_sorted = lt_flights.

    new_line( out ).
    out->write( |Filterd Data on sorted internal table| ).

    DATA(lt_flights_aa) = FILTER ty_t_flight( lt_flights_sorted  WHERE carrier_id = CONV #( 'AA' )  ).
    out->write( lt_flights_aa ).

**************************************************************************************************************
    new_line( out ).
    out->write( |Filterd Data except 'AA'| ).
    lt_flights_sorted = lt_flights.

    DATA(lt_flights_except_aa) = FILTER ty_t_flight( lt_flights_sorted EXCEPT WHERE carrier_id = CONV #( 'AA' )  ).
    out->write( lt_flights_except_aa ).

**************************************************************************************************************
    "https://www.youtube.com/watch?v=MazedVvQYBg
    "Real time example - FOR all entries equivalent for internal tables using FILTER operator.
**************************************************************************************************************
    new_line( out ).

    SELECT FROM /dmo/carrier
    FIELDS carrier_id, currency_code
    WHERE currency_code = 'EUR'
    INTO TABLE @lt_usd_carriers.

    out->write( |*Important: FOR ALL ENTRIES equivalent for internal tables using FILTER operator.| ).
    out->write( |DATA(lt_filterd_flights) = FILTER ty_t_flight( lt_flights_sorted IN lt_usd_carriers WHERE carrier_id = carrier_id ).| ).
    "We want to now show only those flights, whose carriers have EUR as currency.
    DATA(lt_filterd_flights) = FILTER ty_t_flight( lt_flights_sorted IN lt_usd_carriers WHERE carrier_id = carrier_id ).


    out->write( lt_filterd_flights ).
*    data(lt_filterd_flights) = FILTER ty_t_flight( lt_flights_sorted USING KEY primary_key   "for standard tables we can use primary_key
*                                                      IN lt_usd_carriers WHERE carrier_id = carrier_id ).
**************************************************************************************************************
  ENDMETHOD.

  METHOD notes.
    "Shortcuts
    " Ctrl + 1 : Quick assist.
    " Ctrl + Alt + DownArrow : Create a copy of the current line
    " Ctrl + Shift + X : UPPER CASE
    " Ctrl + Shift + Y : loswe case


    "T_Codes:
    "DWDM - Development workbench demos.



  ENDMETHOD.

  METHOD open_sql_enhancements.
*Case statement in Select - Open SQL Enhancement
    sql_case( out ).

*COALESCE function in Select - Open SQL Enhancement
    sql_coalesce( out ).


*Host Expression in where -Open SQL Enhancement
    sql_host_expression( out ).

*Client Handling - Open SQL Enhancement ABAP on HANA


*Cross and Right outer join - Open SQL Enhancement ABAP on HANA
    sql_cross_right_outer_join( out ).

*Group by in detail - Open SQL Enhancement
    "https://www.youtube.com/watch?v=kItAzbHIclw
    sql_groupby_detail( out ).

*Having Clause in Select - Open SQL Enhancement
    sql_having_clause( out ).

*Union and Union all in Select - Open SQL Enhancement
    sql_union( out ).


*Window Expressions in ABAP SQL
    sql_window_expressions( out ).

*CommonTableExpression(CTE) in ABAP SQL
    sql_cte( out ).

  ENDMETHOD.

  METHOD sql_case.
**************************************************************************************************************
*Case statement in Select - Open SQL Enhancement
**************************************************************************************************************

    out->write( |>Case statement in Select - Open SQL Enhancement| ).
    new_line( out ).

    SELECT FROM /dmo/flight
    FIELDS carrier_id,
        CASE currency_code       "Simple case statement
            WHEN 'USD' THEN 'US Dollar'
            WHEN 'SGD' THEN 'Singapore Dollar'
            WHEN 'EUR' THEN 'Euro'
            ELSE 'Other currency'
            END AS currency_description,
        price,
        CASE                     "Complex case statement
            WHEN price > 2500 THEN 'Costly'
            WHEN ( price > 500 AND price < 2500 ) THEN 'Reasonable'
            ELSE 'Cheap'
            END AS Flight_Price,

            CASE                      "Complex case statement
            WHEN  currency_code EQ 'USD' THEN
                   CASE
                    WHEN price > 2500 THEN 'Costly'
                    WHEN ( price > 500 AND price < 2500 ) THEN 'Reasonable'
                    ELSE 'Cheap' END
            WHEN  currency_code EQ 'EUR' THEN
                   CASE
                    WHEN price > 1500 THEN 'Costly'
                    WHEN ( price > 400 AND price < 1500 ) THEN 'Reasonable'
                    ELSE 'Cheap' END
            ELSE
                   CASE
                    WHEN price > 2000 THEN 'Costly'
                    WHEN ( price > 1000 AND price < 2000 ) THEN 'Reasonable'
                    ELSE 'Cheap' END
            END AS currency_adjusted_flight_price

    INTO TABLE @DATA(lt_flights).

    CHECK sy-subrc EQ 0.

    out->write( lt_flights ).

  ENDMETHOD.

  METHOD sql_coalesce.

**************************************************************************************************************
*COALESCE function in Select - Open SQL Enhancement
**************************************************************************************************************
    out->write( |>COALESCE function in Select - Open SQL Enhancement| ).
    new_line( out ).

    " Execute demo class CL_DEMO_SQL_EXPR_COALESCE

*the COALESCE function is used to return the first non-null value from a list of expressions.
*It is commonly used to handle null values and provide default values when working with database tables

  ENDMETHOD.



  METHOD sql_host_expression.
**************************************************************************************************************
*Host Expression in where -Open SQL Enhancement
**************************************************************************************************************
    out->write( |>Host Expression in where -Open SQL Enhancement| ).
    new_line( out ).
**************************************************************************************************************
    SELECT * FROM /dmo/carrier
    INTO TABLE @DATA(lt_carriers).

    SELECT FROM /dmo/flight
    FIELDS carrier_id, connection_id, flight_date
    WHERE carrier_id =
         @( VALUE /dmo/carrier-carrier_id( lt_carriers[ name = 'United Airlines, Inc.' ]-carrier_id OPTIONAL ) )
    INTO TABLE @DATA(lt_result).

    CHECK sy-subrc EQ 0.

    out->write( lt_result ).
  ENDMETHOD.


  METHOD sql_cross_right_outer_join.


**************************************************************************************************************
*Cross and Right outer join - Open SQL Enhancement ABAP on HANA
**************************************************************************************************************
    new_line( out ).
    out->write( |>Cross and Right outer join - Open SQL Enhancement ABAP on HANA| ).
    new_line( out ).
**************************************************************************************************************
    DATA: lt_table1 TYPE TABLE OF sychar01,
          lt_table2 TYPE TABLE OF sychar01.

    lt_table1 = VALUE #( ( 'A' )
                         ( 'B' )
                         ( 'C' )
                          ).

    lt_table2 = VALUE #( ( '1' )
                         ( '2' ) ).


    "Cross join
    SELECT  * FROM
    @lt_table1 AS t1
    CROSS JOIN
    @lt_table2 AS t2
      INTO TABLE @DATA(lt_result2).  "notice no on condition in cross join.

    out->write( lt_result2 ).
    new_line( out ).

    "Earlier as cross join was not available, it was being done as below
    SELECT  * FROM @lt_table1 AS t1
    INNER JOIN @lt_table2 AS t2
    ON 1 = 1
     INTO TABLE @DATA(lt_result3).

    out->write( lt_result3 ).
    new_line( out ).


    SELECT FROM
    /dmo/flight AS flight
    RIGHT OUTER JOIN
    /dmo/carrier AS carrier
    ON flight~carrier_id = carrier~carrier_id
    FIELDS
        flight~carrier_id,
        flight~flight_date,
        carrier~name
       INTO TABLE @DATA(lt_rightouter).

    out->write( lt_rightouter ).

  ENDMETHOD.



  METHOD sql_groupby_detail.

**************************************************************************************************************
*Group by in detail - Open SQL Enhancement
    "https://www.youtube.com/watch?v=kItAzbHIclw
**************************************************************************************************************
    new_line( out ).
    out->write( |>Group by in detail - Open SQL Enhancement| ).
    new_line( out ).

    "Refer to CL_DEMO_SQL_EXPR_WITH_GROUP_BY

    SELECT FROM /dmo/carrier AS carrier
        INNER JOIN /dmo/flight AS flight
        ON carrier~carrier_id = flight~carrier_id
        FIELDS carrier~carrier_id AS carid,
               flight~currency_code AS currency,
*               AVG( flight~price ) AS avg,
*               MAX( flight~price ) AS max,
*               MIN( flight~price ) AS min,
*               COUNT( flight~price ) AS count,
               SUM( flight~price ) AS total_price,
               CASE                     "Complex case statement
                    WHEN flight~price > 2500 THEN 'Costly'
                    WHEN ( flight~price > 500 AND price < 2500 ) THEN 'Reasonable'
                    ELSE 'Cheap'
                    END AS Flight_Price
*        WHERE
*                CASE                     "Complex case statement
*                    WHEN flight~price > 2500 THEN 'Costly'
*                    WHEN ( flight~price > 500 AND price < 2500 ) THEN 'Reasonable'
*                    ELSE 'Cheap'
*                    END  EQ 'Cheap' "NOTICE we have the same expression in SELECT and WHERE clause

        GROUP BY
                carrier~carrier_id,
                flight~currency_code,
*                flight~price "This will lead to much more no of lines
                CASE                     "Complex case statement
                    WHEN flight~price > 2500 THEN 'Costly'
                    WHEN ( flight~price > 500 AND price < 2500 ) THEN 'Reasonable'
                    ELSE 'Cheap'
                    END "NOTICE we have the same expression in SELECT and GROUP BY clause

        ORDER BY carid

        INTO TABLE @DATA(lt_res).

    out->write( lt_res ).

  ENDMETHOD.



  METHOD sql_having_clause.

**************************************************************************************************************
*Having Clause in Select - Open SQL Enhancement
**************************************************************************************************************

    new_line( out ).
    out->write( |>Having Clause in Select - Open SQL Enhancement| ).
    new_line( out ).

    SELECT FROM /dmo/carrier AS carrier
        INNER JOIN /dmo/flight AS flight
        ON carrier~carrier_id = flight~carrier_id
        FIELDS carrier~carrier_id AS carid,
               flight~currency_code AS currency,
               SUM( flight~price ) AS total_price,
               AVG( flight~price ) AS avg,
               MAX( flight~price ) AS max,
               MIN( flight~price ) AS min,
               COUNT( flight~price ) AS count

        GROUP BY
                carrier~carrier_id,
    "we can't use UP TO N rows
    "Table buffer is bypaseed if UNION used.
    "UNION ALL | DISTINCT.
                flight~currency_code
        HAVING SUM( flight~price ) > 30000

        ORDER BY carid
        INTO TABLE @DATA(lt_res1).

    out->write( lt_res1 ).

    " Not necessary to have the field/expression in SELECT list, for it to be used in HAVING clause.
    " However this field/expression should be included in GROUP BY clause, for it it to be used in HAVING clause.


  ENDMETHOD.



  METHOD sql_union.

**************************************************************************************************************
*Union and Union all in Select - Open SQL Enhancement
**************************************************************************************************************
    new_line( out ).
    out->write( |>Union and Union all in Select - Open SQL Enhancement| ).
    new_line( out ).

    SELECT FROM /dmo/carrier AS carrier
        INNER JOIN /dmo/flight AS flight
        ON carrier~carrier_id = flight~carrier_id
        FIELDS carrier~carrier_id AS carid,
               flight~currency_code AS currency,
               SUM( flight~price ) AS total_price,
               AVG( flight~price ) AS avg,
               MAX( flight~price ) AS max,
               MIN( flight~price ) AS min,
               COUNT( flight~price ) AS count,
               'Expensive Flights' AS flight_cat
        GROUP BY
                carrier~carrier_id,
                flight~currency_code
        HAVING SUM( flight~price ) > 30000

*    UNION
UNION DISTINCT
*    UNION ALL

    SELECT FROM /dmo/carrier AS carrier
        INNER JOIN /dmo/flight AS flight
        ON carrier~carrier_id = flight~carrier_id
        FIELDS carrier~carrier_id AS carid,
               flight~currency_code AS currency,
               SUM( flight~price ) AS total_price,
               AVG( flight~price ) AS avg,
               MAX( flight~price ) AS max,
               MIN( flight~price ) AS min,
               COUNT( flight~price ) AS count,
           'Other Flights' AS flight_cat
        GROUP BY
                carrier~carrier_id,
                flight~currency_code

        ORDER BY carid
        INTO TABLE @DATA(lt_res_union).


    out->write( lt_res_union ).

  ENDMETHOD.



  METHOD sql_window_expressions.

**************************************************************************************************************
*Window Expressions in ABAP SQL
**************************************************************************************************************
    new_line( out ).
    out->write( |>Window Expressions in ABAP SQL| ).
    out->write( |>e.g.1| ).
    new_line( out ).

    SELECT
    FROM /dmo/flight
    FIELDS
    carrier_id AS carid,
    connection_id AS conid,
    flight_date AS fldat,
*    price,
*    MAX( price ) AS maxprice
*    LAG( flight_date, 2, '11111111' ) OVER( PARTITION BY carrier_id  ORDER BY connection_id DESCENDING ) AS prev_fldat,
    LAG( flight_date ) OVER( PARTITION BY carrier_id  ORDER BY connection_id DESCENDING ) AS prev_fldat,
    RANK(  ) OVER( PARTITION BY carrier_id  ORDER BY connection_id DESCENDING ) AS rank,
    DENSE_RANK(  ) OVER( PARTITION BY carrier_id  ORDER BY connection_id DESCENDING ) AS denserank,
    ROW_NUMBER(  ) OVER( PARTITION BY carrier_id  ORDER BY connection_id DESCENDING ) AS rn,
    MAX( price ) OVER( PARTITION BY carrier_id ) AS maxprc_by_carr,
    MAX( price ) OVER( ) AS maxprice "Empty OVER clause means sql considers the whole result set as 1 window.


*    GROUP BY carrier_id    "Group by is not needed for window functions
    INTO TABLE @DATA(lt_res_window).

    out->write( lt_res_window ).



    "*Two different ORDER BY in different window functions can lead unexpected sorting .

    new_line( out ).
    out->write( |>Window Expressions in ABAP SQL| ).
    out->write( |>e.g.2| ).
    new_line( out ).

    SELECT
    FROM /dmo/flight
    FIELDS
    carrier_id AS carid,
    connection_id AS conid,
    flight_date AS fldat,
    price,
    SUM( price ) OVER( PARTITION BY carrier_id  ) AS TOT_price_per_carrier1,
    SUM( price ) OVER( PARTITION BY carrier_id ORDER BY price ) AS TOT_price_per_carrier2,
    SUM( price ) OVER( PARTITION BY carrier_id ORDER BY price
                        ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING
                        "https://learnsql.com/blog/sql-window-functions-rows-clause/
                        ) AS TOT_price_per_carrier3
*    COUNT( price ) OVER( PARTITION BY carrier_id ORDER BY price ) AS flights_per_carrier
*    COUNT( price ) OVER( PARTITION BY carrier_id ORDER BY price ) AS flights_per_carrier

    WHERE carrier_id IN ( 'AZ' , 'JL' )
    INTO TABLE @DATA(lt_res_window1).

    out->write( lt_res_window1 ).
    ""https://learnsql.com/blog/sql-window-functions-rows-clause/
*The purpose of the ROWS clause is to specify the window frame in relation to the current row. The syntax is:
*
*ROWS BETWEEN lower_bound AND upper_bound
*
*The bounds can be any of these five options:
*
*    UNBOUNDED PRECEDING – All rows before the current row.
*    n PRECEDING – n rows before the current row.
*    CURRENT ROW – Just the current row.
*    n FOLLOWING – n rows after the current row.
*    UNBOUNDED FOLLOWING – All rows after the current row.

*Here are a couple of things to keep in mind when defining window frames with the ROWS clause:
*
*The window frame is evaluated separately within each partition.

*The default option depends on if you use ORDER BY: ***Important
*   With ORDER BY, the default frame is RANGE BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW.
*   Without ORDER BY, the default frame is ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING.

  ENDMETHOD.


  METHOD sql_cte.
    new_line( out ).
    out->write( |>CommonTableExpression(CTE) in ABAP SQL| ).
    new_line( out ).
    TYPES: BEGIN OF struct,
             carrier_id    TYPE /dmo/flight-carrier_id,
             connection_id TYPE /dmo/flight-connection_id,
             flight_date   TYPE /dmo/flight-flight_date,
             count         TYPE int8,
           END OF struct.
    DATA: itab TYPE TABLE OF struct WITH EMPTY KEY.

*CommonTableExpression(CTE) in ABAP SQL

    WITH
      +conns AS (
        SELECT /dmo/flight~carrier_id, connection_id,flight_date
              FROM /dmo/flight
                JOIN /dmo/carrier ON /dmo/flight~carrier_id = /dmo/carrier~carrier_id
              WHERE /dmo/flight~carrier_id = 'AZ' ),
      +cnts AS (
        SELECT COUNT(*) AS count
               FROM +conns )
      SELECT *
             FROM +cnts
               CROSS JOIN +conns
             ORDER BY carrier_id, connection_id
             INTO CORRESPONDING FIELDS OF TABLE @itab.

    CHECK sy-subrc EQ 0.
    out->write( itab ).
  ENDMETHOD.






































ENDCLASS.
