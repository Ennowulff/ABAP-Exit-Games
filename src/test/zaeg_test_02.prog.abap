REPORT zaeg_test_02.

CLASS aeg1 DEFINITION.
  PUBLIC SECTION.
    METHODS go.
  PRIVATE SECTION.
    METHODS colname
      IMPORTING
        number      TYPE i
      RETURNING
        VALUE(name) TYPE string.
    DATA rdigits TYPE REF TO data.
    DATA salv TYPE REF TO cl_salv_table.
    METHODS fill
      RETURNING VALUE(cols) TYPE i.
    METHODS create_table
      IMPORTING
        width  TYPE i
        height TYPE i.
    METHODS set
      IMPORTING
        VALUE(x) TYPE i
        VALUE(y) TYPE i.
ENDCLASS.

CLASS aeg1 IMPLEMENTATION.
  METHOD go.

    DATA(cols) = fill( ).

    FIELD-SYMBOLS <digits> TYPE STANDARD TABLE.
    ASSIGN rdigits->* TO <digits>.
    CHECK <digits> IS ASSIGNED.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = salv
          CHANGING
            t_table        = <digits> ).
        DATA(columns) = salv->get_columns( ).
        DO cols TIMES.
          DATA(col) = CAST cl_salv_column_table( columns->get_column( CONV #( colname( sy-index ) ) ) ).
          col->set_output_length( 2 ).
          col->set_icon( abap_true ).
          col->set_cell_type( if_salv_c_cell_type=>hotspot ).
        ENDDO.
        salv->get_display_settings( ).
        salv->display( ).
      CATCH cx_salv_msg.
    ENDTRY.

  ENDMETHOD.


  METHOD colname.
    name = |I{ number ALIGN = RIGHT WIDTH = 2 PAD = '0' }|.
  ENDMETHOD.

  METHOD fill.
    DATA(input) =
       `20;19;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0` &&
       `0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1;1;0;0;0;1;1;1;1;0;0;0;1;1;1;1;0` &&
       `0;0;1;1;1;0;0;1;1;1;1;1;1;0;1;1;1;1;1;1;0;1;1;1;1;0;0;1;1;0;0;1;1;0;1;1;0;0;1;1` &&
       `0;0;0;1;1;0;0;0;0;0;0;1;1;0;0;0;0;0;1;1;0;0;0;1;1;0;0;0;0;0;0;1;1;0;0;0;0;0;1;1` &&
       `0;0;0;1;1;0;0;0;0;0;1;1;0;0;0;1;1;1;1;0;0;0;0;1;1;0;0;0;0;1;1;0;0;0;0;1;1;1;1;0` &&
       `0;0;0;1;1;0;0;0;1;1;0;0;0;0;0;0;0;0;1;1;0;0;0;1;1;0;0;1;1;0;0;0;0;0;0;0;0;0;1;1` &&
       `0;0;0;1;1;0;0;1;1;0;0;0;0;0;1;1;0;0;1;1;0;0;1;1;1;1;0;1;1;1;1;1;1;0;1;1;1;1;1;1` &&
       `0;0;1;1;1;1;0;1;1;1;1;1;1;0;0;1;1;1;1;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0` &&
       `0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0` &&
       `0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0`  .

    DATA points TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    SPLIT input AT ';' INTO TABLE points.
    cols = CONV i( points[ 1 ] ).
    DATA(rows) = CONV i( points[ 2 ] ).

    create_table( width = cols height = rows ).

    DATA(idx) = 3.

    DO cols TIMES.
      data(x) = sy-index.
      DO rows TIMES.
        data(y) = sy-index.
        TRY.
            IF points[ idx ] = '1'.
              set( x = x y = y ).
            ENDIF.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
        ADD 1 TO idx.
      ENDDO.
    ENDDO.

  ENDMETHOD.

  METHOD set.

    FIELD-SYMBOLS <digits> TYPE STANDARD TABLE.
    ASSIGN rdigits->* TO <digits>.


    ASSIGN <digits>[ y ] TO FIELD-SYMBOL(<line>).
    IF sy-subrc > 0.
      APPEND INITIAL LINE TO <digits> ASSIGNING <line>.
    ENDIF.
    ASSIGN COMPONENT x OF STRUCTURE <line> TO FIELD-SYMBOL(<point>).
    <point> = icon_led_green.

  ENDMETHOD.


  METHOD create_table.

    DATA(comp) = VALUE cl_abap_structdescr=>component_table(
       FOR i = 1 WHILE i <= width
         ( name = colname( i ) type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'ICON_D' ) ) )
    ).

    TRY.
        DATA(digit) = cl_abap_structdescr=>create(
            p_components = comp
            p_strict     = abap_true ).

        DATA(digits) = cl_abap_tabledescr=>create(
                                 p_line_type  = digit ).

        CREATE DATA rdigits TYPE HANDLE digits.
        FIELD-SYMBOLS <digits> TYPE STANDARD TABLE. "XXX
        ASSIGN rdigits->* TO <digits>. "XXX
        DO height TIMES.
          APPEND INITIAL LINE TO <digits>.
        ENDDO.
      CATCH cx_sy_struct_creation cx_sy_table_creation INTO DATA(err_itab).
        MESSAGE err_itab TYPE 'I'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW aeg1( )->go( ).
