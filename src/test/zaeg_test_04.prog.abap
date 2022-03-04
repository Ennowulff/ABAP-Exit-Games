REPORT zaeg_test_04.

" QUEST 2

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
        VALUE(y) TYPE i
        VALUE(c) TYPE i.
    methods on_click
    for EVENT link_click of cl_salv_events_table
    IMPORTING
      column
      row
      sender.
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
        columns->set_color_column( '_COLOR_' ).
        DO cols TIMES.
          DATA(col) = CAST cl_salv_column_table( columns->get_column( CONV #( colname( sy-index ) ) ) ).
          col->set_output_length( 2 ).
          col->set_icon( abap_true ).
          col->set_cell_type( if_salv_c_cell_type=>hotspot ).
        ENDDO.
        salv->get_display_settings( )->set_vertical_lines( abap_False ).
        salv->get_display_settings( )->set_horizontal_lines( abap_False ).
        salv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>cell ).
        set handler on_click for salv->get_event( ).
*        write: 'HALLO'.
        salv->display( ).
      CATCH cx_salv_msg cx_salv_not_found cx_salv_data_error INTO DATA(err_salv).
        MESSAGE err_salv TYPE 'I'.
    ENDTRY.

  ENDMETHOD.


  METHOD colname.
    name = |I{ number ALIGN = RIGHT WIDTH = 2 PAD = '0' }|.
  ENDMETHOD.

  METHOD fill.
    DATA(input) =
       `623574`.

*    LOOP AT input INTO DATA(line).
*      SPLIT line AT ';' INTO TABLE DATA(points).
*      APPEND INITIAL LINE TO <digits> ASSIGNING FIELD-SYMBOL(<digit_line>).
*      LOOP AT points INTO DATA(point).
*        ASSIGN COMPONENT sy-tabix OF STRUCTURE <digit_line> TO FIELD-SYMBOL(<digit>).
*        <digit> = SWITCH #( point WHEN '0' THEN space ELSE icon_led_green ).
*      ENDLOOP.
*    ENDLOOP.

    cols = 30.
    DATA(rows) = 25.

    create_table( width = cols height = rows ).

    DATA(idx) = 1.

      DO rows TIMES.
        DATA(y) = sy-index.
        DO cols TIMES.
          DATA(x) = sy-index.
          set( x = x y = y c = conv #( input(1) ) ).
          if x mod 5 = 0.
          SHIFT input LEFT BY 1 PLACES CIRCULAR.
          endif.
        ENDDO.
          if y mod 5 = 0.
          SHIFT input LEFT BY 1 PLACES CIRCULAR.
          endif.
      ENDDO.

  ENDMETHOD.

  METHOD set.

    FIELD-SYMBOLS <digits> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <color> TYPE lvc_t_scol.
    ASSIGN rdigits->* TO <digits>.

    ASSIGN COMPONENT '_COLOR_' OF STRUCTURE <digits>[ y ] TO <color>.
    IF line_exists( <color>[ fname = colname( x ) ] ).
      <color>[ fname = colname( x ) ] = VALUE #( fname = colname( x ) color-col = c ).
    ELSE.
      INSERT VALUE #( fname = colname( x ) color-col = c ) INTO TABLE <color>.
    ENDIF.
*    ASSIGN <digits>[ y ] TO FIELD-SYMBOL(<line>).
*    IF sy-subrc > 0.
*      APPEND INITIAL LINE TO <digits> ASSIGNING <line>.
*    ENDIF.
*    ASSIGN COMPONENT x OF STRUCTURE <line> TO FIELD-SYMBOL(<point>).
*    <point> = icon_led_green.

  ENDMETHOD.


  METHOD create_table.

    DATA(comp) = VALUE cl_abap_structdescr=>component_table(
       FOR i = 1 WHILE i <= width
         ( name = colname( i ) type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'ICON_D' ) ) )
    ).
    APPEND VALUE #( name = '_COLOR_'
                    type = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_name( 'LVC_T_SCOL' ) ) ) TO comp.

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

  method on_click.
    MESSAGE column type 'S'.
*    salv->get_selections( )->set_current_cell( value = value #( ) ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW aeg1( )->go( ).
