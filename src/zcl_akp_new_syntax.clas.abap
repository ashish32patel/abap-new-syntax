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
    corresponding_operator( out ).

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

    "in case of itabs if any column matches than , first target itab is cleared
    "KEEPING TARGET LINES will not clear the target itab but will append to it

**Example MOVE-CORRESPONDING for Structures        DEMO_MOVE_CORRESPONDING_STRUCT
**Example MOVE-CORRESPONDING for Internal Tables   DEMO_MOVE_CORRESPONDING_STRUCT
**Example Component Operator for Structures        DEMO_CORRESPONDING_STRUCT
**Example Component Operator for Internal Tables   DEMO_CORRESPONDING_ITAB

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
    "compare with the operations in move_corresponding.
    DATA:
      ls_struct1 TYPE ty_struct1,
      ls_struct2 TYPE ty_struct2.


*    clear_fill_structures(
*      CHANGING
*        cs_struct2 = ls_struct2
*        cs_struct1 = ls_struct1 ).
*
*    me->display_structure1( out = out is_struct1 = ls_struct1 ).
*    me->display_structure2( out = out is_struct2 = ls_struct2 ).
***********************************************************************
*    out->write( '>> ls_struct2 = CORRESPONDING #( ls_struct1 ).' ).
*    out->write( |Note: It's not same as MOVE-CORRESPONDING notice COL4 for difference \n    as it target stucture is first overridden| ).
*
*    ls_struct2 = CORRESPONDING #( ls_struct1 ).
*    me->display_structure2( out = out is_struct2 = ls_struct2 ).
***********************************************************************
*    clear_fill_structures( CHANGING cs_struct2 = ls_struct2 cs_struct1 = ls_struct1 ).
*    out->write( '>> ls_struct2 = CORRESPONDING #( BASE ( ls_struct2 ) ls_struct1 ).' ).
*    out->write( |Note: It's same as MOVE-CORRESPONDING| ).
*
*    ls_struct2 = CORRESPONDING #( BASE ( ls_struct2 ) ls_struct1 ).
*    me->display_structure2( out = out is_struct2 = ls_struct2 ).
***********************************************************************
*    clear_fill_structures( CHANGING cs_struct2 = ls_struct2 cs_struct1 = ls_struct1 ).
*    out->write( 'MOVE-CORRESPONDING ls_struct1 to ls_struct2 EXPANDING NESTED TABLES.' ).
*
*    MOVE-CORRESPONDING ls_struct1 TO ls_struct2 EXPANDING NESTED TABLES.
*    me->display_structure2( out = out is_struct2 = ls_struct2 ).
***********************************************************************
*    clear_fill_structures( CHANGING cs_struct2 = ls_struct2 cs_struct1 = ls_struct1 ).
*    out->write( 'ls_struct2 = CORRESPONDING #( DEEP BASE ( ls_struct2 ) ls_struct1 ).' ).
*
*    ls_struct2 = CORRESPONDING #( DEEP BASE ( ls_struct2 ) ls_struct1 ).
*    me->display_structure2( out = out is_struct2 = ls_struct2 ).
**********************************************************************
    DATA: lt_tab1 TYPE  tt_tab1,
          lt_tab2 TYPE tt_tab2.

    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).

    out->write( '===============Begin TABLE section===============' ).
    display_tab1( it_tab1 = lt_tab1 out = out ).
    display_tab2( it_tab2 = lt_tab2 out = out ).
**********************************************************************
    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).
    out->write( '>>lt_tab2 = CORRESPONDING #( lt_tab1 ).' ).
    lt_tab2 = CORRESPONDING #( lt_tab1 ).
    display_tab2( it_tab2 = lt_tab2 out = out ).

**********************************************************************
    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).
    out->write( '>>lt_tab2 = CORRESPONDING #( BASE ( lt_tab2 ) lt_tab1 ).' ).
    lt_tab2 = CORRESPONDING #( BASE ( lt_tab2 ) lt_tab1 ).
    display_tab2( it_tab2 = lt_tab2 out = out ).

**********************************************************************
    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).

    MOVE-CORRESPONDING lt_tab1 TO lt_tab2 KEEPING TARGET LINES.
    out->write( '>>MOVE-CORRESPONDING lt_tab1 to lt_tab2 KEEPING TARGET LINES.' ).
    display_tab2( it_tab2 = lt_tab2 out = out ).
**********************************************************************
    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).
    out->write( '>>lt_tab2 = CORRESPONDING #( DEEP BASE ( lt_tab2 ) lt_tab1 ).' ).
    lt_tab2 = CORRESPONDING #( DEEP BASE ( lt_tab2 ) lt_tab1 ).
    display_tab2( it_tab2 = lt_tab2 out = out ).

**********************************************************************
    clear_fill_itabs( CHANGING ct_tab1 = lt_tab1 ct_tab2 = lt_tab2 ).

    MOVE-CORRESPONDING lt_tab1 TO lt_tab2 EXPANDING NESTED TABLES KEEPING TARGET LINES.
    out->write( '>>MOVE-CORRESPONDING lt_tab1 to lt_tab2 EXPANDING NESTED TABLES KEEPING TARGET LINES.' ).
    display_tab2( it_tab2 = lt_tab2 out = out ).
**********************************************************************
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
    out->write( 'alpha conversions:' ).
    out->write( |{ lv_matnr }| ).
    out->write( |{ lv_matnr ALPHA = OUT }| ).

    out->write( 'DATE/TIME/NUM conversions:' ).
    out->write( |{ sy-datum }| ).
    out->write( |{ sy-datum DATE = USER }| ).

    out->write( |{ sy-uzeit }| ).
    out->write( |{ sy-uzeit TIME = USER }| ).


    DATA(lv_num) = VALUE int4( ).
    lv_num = 13534.
    out->write( |{ lv_num }| ).
    out->write( |{ lv_num NUMBER = USER }| ).

    out->write( |string operations:| ).
    out->write( |{ lc_val }\n{ lc_val CASE = UPPER }| ).  "\n - control character, in the same cell new line added
    "\t - control character adds a tab

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

ENDCLASS.
