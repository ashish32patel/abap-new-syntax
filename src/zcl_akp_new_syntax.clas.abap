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
    reduce_operator( out ).

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

ENDCLASS.
