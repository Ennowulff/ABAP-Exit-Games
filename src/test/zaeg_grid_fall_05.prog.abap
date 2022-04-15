REPORT zaeg_grid_fall_05.


CLASS gf5 DEFINITION.
  PUBLIC SECTION.
    METHODS go.
    DATA docker TYPE REF TO cl_gui_docking_container.
  PROTECTED SECTION.
    DATA app1 TYPE REF TO cl_gui_alv_grid.
    DATA icons TYPE STANDARD TABLE OF icon.

ENDCLASS.

CLASS gf5 IMPLEMENTATION.

  METHOD go.

    SELECT * FROM icon INTO TABLE icons UP TO 40 ROWS.
    docker = NEW #( ratio = 80 side = cl_gui_docking_container=>dock_at_bottom ).

    DATA assigned_url TYPE c LENGTH 1000.
    DATA solix        TYPE solix_tab.

    DATA content_type TYPE  w3param-cont_type.
    DATA content_length TYPE  w3param-cont_len.

    docker->set_mode( cl_gui_control=>mode_design ).
    app1 = NEW #( i_parent = docker ).
    app1->set_adjust_design( 1 ).

    DATA x TYPE xstring.
    x = '424D2E010000000000003E00000028000000320000001E000000010001000000'
     && '0000F0000000C40E0000C40E0000000000000000000000000000FFFFFF00FFFF'
     && 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC1FFFFFFFFFFFFFF80'
     && 'FFFFE007FFFFFFC0FFE78003FFFFFFE1FFC78003FFFFFFE3FFC70FE1FFFFFFE3'
     && 'FFC71FE1FFFFFFE3FFC71FF1FFFFFFE3FFC71FF1FFFFFFE3C0071FF1FFFFFFE3'
     && '80030FF1FFFFFFE380030FF1FFFFFFE38FC383F1FFFFFFE38FC7C001FFFFFFE3'
     && '8FC7E003FFFFF8638FC7E001FFFFF0238FC3E001FFFFF8038FC3C3F1FFFFFE03'
     && '8FE3C7F1FFFFFF038FE3C3F1FFFFFF838FE3C0F1FFFFFFC30FE3E001FFFFFFE7'
     && '0FE3F003FFFFFFFF9FF7FC1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
     && 'FFFFFFFFFFFFFFFFFFFFFFFFFFFF'.

    DATA(appl) = NEW cl_gui_picture( parent = docker ).
    solix = cl_bcs_convert=>xstring_to_solix( x ).
    content_length = xstrlen( x ).

    CALL FUNCTION 'DP_CREATE_URL'
      EXPORTING
        type     = 'image'
        subtype  = cndp_sap_tab_unknown
        size     = content_length
        lifetime = cndp_lifetime_transaction
      TABLES
        data     = solix
      CHANGING
        url      = assigned_url
      EXCEPTIONS
        OTHERS   = 1.

    appl->load_picture_from_url( assigned_url ).
    appl->set_display_mode( cl_gui_picture=>display_mode_fit_center ).


    app1->set_mode( cl_gui_alv_grid=>mode_design ).
    app1->set_table_for_first_display(
      EXPORTING
        i_structure_name              = 'ICON'
        is_layout                     = VALUE lvc_s_layo( no_headers = abap_false )
      CHANGING
        it_outtab                     = icons
      EXCEPTIONS
        OTHERS                        = 4 ).
  ENDMETHOD.

ENDCLASS.

PARAMETERS test.

INITIALIZATION.
  NEW gf5( )->go( ).
