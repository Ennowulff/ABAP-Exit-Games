REPORT zaeg_grid_fall_02.

" GRID FALL - QUEST 2

CLASS aeg1 DEFINITION.
  PUBLIC SECTION.
    METHODS go.
  PRIVATE SECTION.

    TYPES: _type TYPE c LENGTH 1,
           _area TYPE c LENGTH 1.
    CONSTANTS _type_upper    TYPE _type VALUE 'U'.
    CONSTANTS _type_original TYPE _type VALUE 'O'.
    CONSTANTS _type_land     TYPE _type VALUE 'L'.
    CONSTANTS _type_text     TYPE _type VALUE 'T'.
    CONSTANTS _type_internal TYPE _type VALUE 'I'.
    CONSTANTS _type_sprint   TYPE _type VALUE 'S'.
    CONSTANTS _type_new      TYPE _type VALUE 'N'.
    CONSTANTS _area_internal TYPE _area VALUE 'I'.
    CONSTANTS _area_external TYPE _area VALUE 'E'.


    TYPES: BEGIN OF _line,
             mat    TYPE c LENGTH 10,
             type   TYPE _type,
             area   TYPE _area,
             amount TYPE p LENGTH 5 DECIMALS 0,
           END OF _line,
           _data TYPE STANDARD TABLE OF _line WITH EMPTY KEY.


    DATA salv TYPE REF TO cl_salv_table.
    DATA data TYPE _data.

    METHODS get_data.
ENDCLASS.

CLASS aeg1 IMPLEMENTATION.
  METHOD go.

    get_data( ).

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = salv
          CHANGING
            t_table        = data ).
        DATA(columns) = salv->get_columns( ).

        salv->get_columns( )->get_column( 'MAT' )->set_medium_text( 'Material' ).
        salv->get_columns( )->get_column( 'AREA' )->set_medium_text( 'Area' ).
        salv->get_columns( )->get_column( 'TYPE' )->set_medium_text( 'Type' ).
        salv->get_columns( )->get_column( 'AMOUNT' )->set_medium_text( 'Amount' ).

        salv->get_filters( )->add_filter( columnname = 'area' sign = 'I' option = 'EQ' low = 'l' ).
        salv->get_sorts( )->add_sort( 'mat' ).
        salv->get_aggregations( )->add_aggregation(
            columnname  = 'AMOUNT'
            aggregation = if_salv_c_aggregation=>total  ).
      CATCH cx_salv_msg cx_salv_not_found cx_salv_existing cx_salv_data_error INTO DATA(err_salv).
        "exception handling
    ENDTRY.
    salv->display( ).

  ENDMETHOD.


  METHOD get_data.

    data = VALUE #(
       ( mat = 'EE' type = _type_internal area = _area_internal amount = 555 )
       ( mat = 'BB' type = _type_land     area = _area_internal amount = 222 )
       ( mat = 'AA' type = _type_original area = _area_internal amount = 111 )
       ( mat = 'HH' type = _type_original area = _area_external amount = 888 )
       ( mat = 'FF' type = _type_original area = _area_internal amount = 666 )
       ( mat = 'DD' type = _type_text     area = _area_internal amount = 444 )
       ( mat = 'GG' type = _type_new      area = _area_internal amount = 777 )
       ( mat = 'CC' type = _type_upper    area = _area_internal amount = 333 )
       ( mat = 'A2' type = _type_sprint   area = _area_internal amount = 111 )

          ).

  ENDMETHOD.


ENDCLASS.

START-OF-SELECTION.
  NEW aeg1( )->go( ).
