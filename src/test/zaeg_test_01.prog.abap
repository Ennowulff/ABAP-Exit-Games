REPORT zaeg_test_01.

CLASS aeg1 DEFINITION.
  PUBLIC SECTION.
    METHODS go.
  PRIVATE SECTION.
    METHODS click FOR EVENT link_click OF cl_salv_events_table IMPORTING column row.
    METHODS colname
      IMPORTING
        number      TYPE i
      RETURNING
        VALUE(name) TYPE string.
    DATA rdigits TYPE REF TO data.
    DATA salv TYPE REF TO cl_salv_table.
ENDCLASS.

CLASS aeg1 IMPLEMENTATION.
  METHOD go.

    FIELD-SYMBOLS <digits> TYPE STANDARD TABLE.
    DATA(comp) = VALUE cl_abap_structdescr=>component_table(
       FOR i = 1 WHILE i <= 50
         ( name = colname( i ) type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'ICON_D' ) ) )
    ).

    TRY.
        DATA(digit) = cl_abap_structdescr=>create(
            p_components = comp
            p_strict     = abap_true ).

        DATA(digits) = cl_abap_tabledescr=>create(
                                 p_line_type  = digit ).

        CREATE DATA rdigits TYPE HANDLE digits.
        ASSIGN rdigits->* TO <digits>.
      CATCH cx_sy_struct_creation cx_sy_table_creation INTO DATA(err_itab).
        MESSAGE err_itab TYPE 'I'.
    ENDTRY.

    CHECK <digits> IS ASSIGNED.

    DO 12 TIMES.
      APPEND INITIAL LINE TO <digits> ASSIGNING FIELD-SYMBOL(<digit>) .
    ENDDO.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = salv
          CHANGING
            t_table        = <digits> ).
        DATA(columns) = salv->get_columns( ).
        DO 50 TIMES.
          DATA(col) = CAST cl_salv_column_table( columns->get_column( |I{ sy-index ALIGN = RIGHT WIDTH = 2 PAD = '0' }| ) ).
          col->set_output_length( 2 ).
          col->set_icon( abap_true ).
          col->set_cell_type( if_salv_c_cell_type=>hotspot ).
        ENDDO.
        SET HANDLER click FOR salv->get_event( ).
        salv->display( ).
      CATCH cx_salv_msg.
    ENDTRY.

  ENDMETHOD.


  METHOD click.
    FIELD-SYMBOLS <digits> TYPE STANDARD TABLE.
    ASSIGN rdigits->* TO <digits>.
    ASSIGN COMPONENT column OF STRUCTURE <digits>[ row ] TO FIELD-SYMBOL(<col>).
    <col> = switch #( <col> when space then icon_led_green else space ).
    salv->refresh( ).
  ENDMETHOD.
  METHOD colname.
    name = |I{ number ALIGN = RIGHT WIDTH = 2 PAD = '0' }|.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW aeg1( )->go( ).
