CLASS lhc_Travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF gc_travel_status,
        open     TYPE c LENGTH 1  VALUE 'O', " Open
        accepted TYPE c LENGTH 1  VALUE 'A', " Accepted
        canceled TYPE c LENGTH 1  VALUE 'X', " Cancelled
      END OF gc_travel_status.

    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~calculateTotalPrice.

    METHODS calculateTravelID FOR DETERMINE ON SAVE
      IMPORTING keys FOR Travel~calculateTravelID.

    METHODS setInitialStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~setInitialStatus.

    METHODS validateAgency FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateAgency.

    METHODS validateCustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateCustomer.

    METHODS validateDates FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateDates.

    METHODS acceptTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~acceptTravel RESULT result.

    METHODS rejectTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~rejectTravel RESULT result.

    METHODS recalcTotalPrice FOR MODIFY
      IMPORTING keys FOR ACTION Travel~recalcTotalPrice.

    METHODS get_features FOR FEATURES
      IMPORTING keys REQUEST requested_features FOR Travel RESULT result.

    METHODS get_authorizations FOR AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Travel RESULT result.

    METHODS is_update_granted IMPORTING iv_has_before_image         TYPE abap_bool
                                        iv_status                   TYPE /dmo/travel_status
                              RETURNING VALUE(rv_is_update_granted) TYPE abap_bool.

    METHODS is_delete_granted IMPORTING iv_has_before_image         TYPE abap_bool
                                        iv_status                   TYPE /dmo/travel_status
                              RETURNING VALUE(rv_is_delete_granted) TYPE abap_bool.

    METHODS is_create_granted RETURNING VALUE(rv_is_create_granted) TYPE abap_bool.

ENDCLASS.

CLASS lhc_Travel IMPLEMENTATION.

  METHOD calculateTotalPrice.
    MODIFY ENTITIES OF zi_rap_ib_travel IN LOCAL MODE
          ENTITY travel
            EXECUTE recalcTotalPrice
            FROM CORRESPONDING #( keys )
          REPORTED DATA(lt_execute_reported).

    reported = CORRESPONDING #( DEEP lt_execute_reported ).
  ENDMETHOD.

  METHOD calculateTravelID.
    " Please note that this is just an example for calculating a field during _onSave_.
    " This approach does NOT ensure for gap free or unique travel IDs! It just helps to provide a readable ID.
    " The key of this business object is a UUID, calculated by the framework.

    " check if TravelID is already filled
    READ ENTITIES OF zi_rap_ib_travel IN LOCAL MODE
      ENTITY Travel
        FIELDS ( TravelID ) WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).

    " remove lines where TravelID is already filled.
    DELETE lt_travels WHERE TravelID IS NOT INITIAL.

    " anything left ?
    CHECK lt_travels IS NOT INITIAL.

    " Select max travel ID
    SELECT SINGLE
        FROM  zrap_ib_atrav
        FIELDS MAX( travel_id ) AS travelID
        INTO @DATA(max_travelid).

    " Set the travel ID
    MODIFY ENTITIES OF zi_rap_ib_travel IN LOCAL MODE
        ENTITY Travel
          UPDATE
            FROM VALUE #( FOR ls_travel IN lt_travels INDEX INTO i (
              %tky              = ls_travel-%tky
              TravelID          = max_travelid + i
              %control-TravelID = if_abap_behv=>mk-on ) )
            REPORTED DATA(lt_update_reported).

    reported = CORRESPONDING #( DEEP lt_update_reported ).
  ENDMETHOD.

  METHOD setInitialStatus.
    " Read relevant travel instance data
    READ ENTITIES OF zi_rap_ib_travel IN LOCAL MODE
      ENTITY Travel
        FIELDS ( TravelStatus ) WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).

    " Remove all travel instance data with defined status
    DELETE lt_travels WHERE TravelStatus IS NOT INITIAL.
    CHECK lt_travels IS NOT INITIAL.

    " Set default travel status
    MODIFY ENTITIES OF zi_rap_ib_travel IN LOCAL MODE
    ENTITY Travel
      UPDATE
        FIELDS ( TravelStatus )
        WITH VALUE #( FOR ls_travel IN lt_travels
                      ( %tky         = ls_travel-%tky
                        TravelStatus = gc_travel_status-open ) )
    REPORTED DATA(lt_update_reported).

    reported = CORRESPONDING #( DEEP lt_update_reported ).
  ENDMETHOD.

  METHOD validateAgency.
    DATA lt_agencies TYPE SORTED TABLE OF /dmo/agency WITH UNIQUE KEY agency_id.
    " Read relevant travel instance data
    READ ENTITIES OF zi_rap_ib_travel IN LOCAL MODE
      ENTITY Travel
        FIELDS ( AgencyID )
        WITH CORRESPONDING #( keys ) "WITH - instead of WHERE
      RESULT DATA(lt_travels).

    " Optimization of DB select: extract distinct non-initial agency IDs
    lt_agencies = CORRESPONDING #( lt_travels DISCARDING DUPLICATES MAPPING agency_id = AgencyID EXCEPT * ).
    DELETE lt_agencies WHERE agency_id IS INITIAL.

*    IF lt_agencies IS NOT INITIAL.
*      " Check if agency ID exist
*      SELECT FROM /dmo/agency FIELDS agency_id
*        FOR ALL ENTRIES IN @lt_agencies
*        WHERE agency_id = @lt_agencies-agency_id
*        INTO TABLE @DATA(lt_agencies_db).
*    ENDIF.
*
*    " Raise msg for non existing and initial agencyID
*    LOOP AT lt_travels INTO DATA(ls_travel).
*      " Clear state messages that might exist
*      APPEND VALUE #(  %tky               = ls_travel-%tky
*                       %state_area        = 'VALIDATE_AGENCY' )
*        TO reported-travel.
*
*      IF ls_travel-AgencyID IS INITIAL OR NOT line_exists( lt_agencies_db[ agency_id = ls_travel-AgencyID ] ).
*        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.
*
*        APPEND VALUE #( %tky        = ls_travel-%tky
*                        %state_area = 'VALIDATE_AGENCY'
*                        %msg        = NEW zcm_rap_ib(
*                                          severity = if_abap_behv_message=>severity-error
*                                          textid   = zcm_rap_ib=>agency_unknown
*                                          agencyid = ls_travel-AgencyID )
*                        %element-AgencyID = if_abap_behv=>mk-on )
*          TO reported-travel.
*      ENDIF.
*    ENDLOOP.

    LOOP AT lt_travels INTO DATA(travel).
*      " Clear state messages that might exist
      APPEND VALUE #(  %tky               = travel-%tky
                       %state_area        = 'VALIDATE_AGENCY' )
        TO reported-travel.
    ENDLOOP.

    DATA filter_conditions  TYPE if_rap_query_filter=>tt_name_range_pairs .
    DATA ranges_table TYPE if_rap_query_filter=>tt_range_option .
    DATA business_data TYPE TABLE OF zz_travel_agency_es5.

    IF  lt_agencies IS NOT INITIAL.

      ranges_table = VALUE #( FOR agency IN lt_agencies (  sign = 'I' option = 'EQ' low = agency-agency_id ) ).
      filter_conditions = VALUE #( ( name = 'AGENCYID'  range = ranges_table ) ).



      TRY.
          "skip and top must not be used
          "but an appropriate filter will be provided
          NEW zcl_ce_rap_agency_ib( )->get_agencies(
            EXPORTING
              filter_cond    = filter_conditions
              is_data_requested  = abap_true
              is_count_requested = abap_false
            IMPORTING
              business_data  = business_data
            ) .

        CATCH /iwbep/cx_cp_remote
              /iwbep/cx_gateway
              cx_web_http_client_error
              cx_http_dest_provider_error

       INTO DATA(exception).

          DATA(exception_message) = cl_message_helper=>get_latest_t100_exception( exception )->if_message~get_text( ) .

          LOOP AT lt_travels INTO travel.
            APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

            APPEND VALUE #( %tky        = travel-%tky
                            %state_area = 'VALIDATE_AGENCY'
                            %msg        =  new_message_with_text( severity = if_abap_behv_message=>severity-error text = exception_message )
                            %element-AgencyID = if_abap_behv=>mk-on )
              TO reported-travel.
          ENDLOOP.

          RETURN.

      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD validateCustomer.
    " Read relevant travel instance data
    READ ENTITIES OF zi_rap_ib_travel IN LOCAL MODE
      ENTITY Travel
        FIELDS ( CustomerID ) WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).

    DATA lt_customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    lt_customers = CORRESPONDING #( lt_travels DISCARDING DUPLICATES MAPPING customer_id = CustomerID EXCEPT * ).
    DELETE lt_customers WHERE customer_id IS INITIAL.
    IF lt_customers IS NOT INITIAL.
      " Check if customer ID exist
      SELECT FROM /dmo/customer FIELDS customer_id
        FOR ALL ENTRIES IN @lt_customers
        WHERE customer_id = @lt_customers-customer_id
        INTO TABLE @DATA(lt_customers_db).
    ENDIF.

    " Raise msg for non existing and initial customerID
    LOOP AT lt_travels INTO DATA(ls_travel).
      " Clear state messages that might exist
      APPEND VALUE #(  %tky        = ls_travel-%tky
                       %state_area = 'VALIDATE_CUSTOMER' )
        TO reported-travel.

      IF ls_travel-CustomerID IS INITIAL OR NOT line_exists( lt_customers_db[ customer_id = ls_travel-CustomerID ] ).
        APPEND VALUE #(  %tky = ls_travel-%tky ) TO failed-travel.

        APPEND VALUE #(  %tky        = ls_travel-%tky
                         %state_area = 'VALIDATE_CUSTOMER'
                         %msg        = NEW zcm_rap_ib(
                                           severity   = if_abap_behv_message=>severity-error
                                           textid     = zcm_rap_ib=>customer_unknown
                                           customerid = ls_travel-CustomerID )
                         %element-CustomerID = if_abap_behv=>mk-on )
          TO reported-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateDates.
    " Read relevant travel instance data
    READ ENTITIES OF zi_rap_ib_travel IN LOCAL MODE
      ENTITY Travel
        FIELDS ( TravelID BeginDate EndDate ) WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).

    LOOP AT lt_travels INTO DATA(ls_travel).
      " Clear state messages that might exist
      APPEND VALUE #(  %tky        = ls_travel-%tky
                       %state_area = 'VALIDATE_DATES' )
        TO reported-travel.

      IF ls_travel-EndDate < ls_travel-BeginDate.
        APPEND VALUE #( %tky = ls_travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky               = ls_travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                        %msg               = NEW zcm_rap_ib(
                                                 severity  = if_abap_behv_message=>severity-error
                                                 textid    = zcm_rap_ib=>date_interval
                                                 begindate = ls_travel-BeginDate
                                                 enddate   = ls_travel-EndDate
                                                 travelid  = ls_travel-TravelID )
                        %element-BeginDate = if_abap_behv=>mk-on
                        %element-EndDate   = if_abap_behv=>mk-on ) TO reported-travel.

      ELSEIF ls_travel-BeginDate < cl_abap_context_info=>get_system_date( ).
        APPEND VALUE #( %tky               = ls_travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky               = ls_travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                        %msg               = NEW zcm_rap_ib(
                                                 severity  = if_abap_behv_message=>severity-error
                                                 textid    = zcm_rap_ib=>begin_date_before_system_date
                                                 begindate = ls_travel-BeginDate )
                        %element-BeginDate = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD acceptTravel.
    " Set the new overall status
    MODIFY ENTITIES OF zi_rap_ib_travel IN LOCAL MODE
      ENTITY Travel
         UPDATE
           FIELDS ( TravelStatus )
           WITH VALUE #( FOR ls_key IN keys
                           ( %tky         = ls_key-%tky "%tky - same as %key in non-draft use cases
                             TravelStatus = gc_travel_status-accepted ) )
      FAILED failed
      REPORTED reported.

    " Fill the response table
    READ ENTITIES OF zi_rap_ib_travel IN LOCAL MODE " IN LOCAL MODE -> allows modif. of read only fields, skips authorization tests...
      ENTITY Travel
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).

    result = VALUE #( FOR ls_travel IN lt_travels
                        ( %tky   = ls_travel-%tky "%tky - same as %key in non-draft use cases
                          %param = ls_travel ) ).
  ENDMETHOD.

  METHOD rejectTravel.
    " Set the new overall status
    MODIFY ENTITIES OF zi_rap_ib_travel IN LOCAL MODE
      ENTITY Travel
         UPDATE
           FIELDS ( TravelStatus )
           WITH VALUE #( FOR ls_key IN keys
                           ( %tky         = ls_key-%tky "%tky - same as %key in non-draft use cases
                             TravelStatus = gc_travel_status-canceled ) )
      FAILED failed
      REPORTED reported.

    " Fill the response table
    READ ENTITIES OF zi_rap_ib_travel IN LOCAL MODE " IN LOCAL MODE -> allows modif. of read only fields, skips authorization tests...
      ENTITY Travel
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).

    result = VALUE #( FOR ls_travel IN lt_travels
                        ( %tky   = ls_travel-%tky "%tky - same as %key in non-draft use cases
                          %param = ls_travel ) ).
  ENDMETHOD.

  METHOD recalcTotalPrice.
    TYPES: BEGIN OF ty_amount_per_currencycode,
             amount        TYPE /dmo/total_price,
             currency_code TYPE /dmo/currency_code,
           END OF ty_amount_per_currencycode.

    DATA: lt_amount_per_currencycode TYPE STANDARD TABLE OF ty_amount_per_currencycode.

    " Read all relevant travel instances.
    READ ENTITIES OF zi_rap_ib_travel IN LOCAL MODE
          ENTITY Travel
             FIELDS ( BookingFee CurrencyCode )
             WITH CORRESPONDING #( keys )
          RESULT DATA(lt_travels).

    DELETE lt_travels WHERE CurrencyCode IS INITIAL.

    LOOP AT lt_travels ASSIGNING FIELD-SYMBOL(<ls_travel>).
      " Set the start for the calculation by adding the booking fee.
      lt_amount_per_currencycode = VALUE #( ( amount        = <ls_travel>-BookingFee
                                              currency_code = <ls_travel>-CurrencyCode ) ).
      " Read all associated bookings and add them to the total price.
      READ ENTITIES OF zi_rap_ib_travel IN LOCAL MODE
         ENTITY Travel BY \_Booking
            FIELDS ( FlightPrice CurrencyCode )
          WITH VALUE #( ( %tky = <ls_travel>-%tky ) )
          RESULT DATA(lt_bookings).
      LOOP AT lt_bookings INTO DATA(ls_booking) WHERE CurrencyCode IS NOT INITIAL.
        COLLECT VALUE ty_amount_per_currencycode( amount        = ls_booking-FlightPrice
                                                  currency_code = ls_booking-CurrencyCode ) INTO lt_amount_per_currencycode.
      ENDLOOP.

      CLEAR <ls_travel>-TotalPrice.
      LOOP AT lt_amount_per_currencycode INTO DATA(ls_single_amount_per_curr_code).
        " If needed do a Currency Conversion
        IF ls_single_amount_per_curr_code-currency_code = <ls_travel>-CurrencyCode.
          <ls_travel>-TotalPrice += ls_single_amount_per_curr_code-amount.
        ELSE.
          /dmo/cl_flight_amdp=>convert_currency(
             EXPORTING
               iv_amount                   =  ls_single_amount_per_curr_code-amount
               iv_currency_code_source     =  ls_single_amount_per_curr_code-currency_code
               iv_currency_code_target     =  <ls_travel>-CurrencyCode
               iv_exchange_rate_date       =  cl_abap_context_info=>get_system_date( )
             IMPORTING
               ev_amount                   = DATA(total_booking_price_per_curr)
            ).
          <ls_travel>-TotalPrice += total_booking_price_per_curr.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    " write back the modified total_price of lt_travels
    MODIFY ENTITIES OF zi_rap_ib_travel IN LOCAL MODE
      ENTITY travel
        UPDATE FIELDS ( TotalPrice )
        WITH CORRESPONDING #( lt_travels ).
  ENDMETHOD.

  METHOD get_features.
    " Read the travel status of the existing travels
    READ ENTITIES OF zi_rap_ib_travel IN LOCAL MODE
      ENTITY Travel
        FIELDS ( TravelStatus ) WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels)
      FAILED failed.

    result =
      VALUE #(
        FOR ls_travel IN lt_travels
          LET lv_is_accepted_enabled =   COND #( WHEN ls_travel-TravelStatus = gc_travel_status-accepted
                                                 THEN if_abap_behv=>fc-o-disabled
                                                 ELSE if_abap_behv=>fc-o-enabled  )
              lv_is_rejected_enabled =   COND #( WHEN ls_travel-TravelStatus = gc_travel_status-canceled
                                                 THEN if_abap_behv=>fc-o-disabled
                                                 ELSE if_abap_behv=>fc-o-enabled )
          IN
            ( %tky                 = ls_travel-%tky
              %action-acceptTravel = lv_is_accepted_enabled
              %action-rejectTravel = lv_is_rejected_enabled
             ) ).
  ENDMETHOD.

  METHOD get_authorizations.
    DATA: lv_has_before_image    TYPE abap_bool,
          lv_is_update_requested TYPE abap_bool,
          lv_is_delete_requested TYPE abap_bool,
          lv_is_update_granted   TYPE abap_bool,
          lv_is_delete_granted   TYPE abap_bool.

    DATA: ls_failed_travel LIKE LINE OF failed-travel.

    " Read the existing travels
    READ ENTITIES OF zi_rap_ib_travel IN LOCAL MODE
      ENTITY Travel
        FIELDS ( TravelStatus ) WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels)
      FAILED failed.

    CHECK lt_travels IS NOT INITIAL.

*   In this example the authorization is defined based on the Activity + Travel Status
*   For the Travel Status we need the before-image from the database. We perform this for active (is_draft=00) as well as for drafts (is_draft=01) as we can't distinguish between edit or new drafts
    SELECT FROM zrap_ib_atrav
      FIELDS travel_uuid,status
      FOR ALL ENTRIES IN @lt_travels
      WHERE travel_uuid EQ @lt_travels-TravelUUID
      ORDER BY PRIMARY KEY
      INTO TABLE @DATA(lt_travels_before_image).

    lv_is_update_requested = COND #( WHEN requested_authorizations-%update              = if_abap_behv=>mk-on OR
                                          requested_authorizations-%action-acceptTravel = if_abap_behv=>mk-on OR
                                          requested_authorizations-%action-rejectTravel = if_abap_behv=>mk-on OR
                                          requested_authorizations-%action-Prepare      = if_abap_behv=>mk-on OR  "for draft
                                          requested_authorizations-%action-Edit         = if_abap_behv=>mk-on OR  "for draft
                                          requested_authorizations-%assoc-_Booking      = if_abap_behv=>mk-on
                                     THEN abap_true ELSE abap_false ).

    lv_is_delete_requested = COND #( WHEN requested_authorizations-%delete = if_abap_behv=>mk-on
                                  THEN abap_true ELSE abap_false ).

    LOOP AT lt_travels INTO DATA(travel).
      lv_is_update_granted = lv_is_delete_granted = abap_false.

      READ TABLE lt_travels_before_image INTO DATA(travel_before_image)
       WITH KEY travel_uuid = travel-TravelUUID BINARY SEARCH.
      lv_has_before_image = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

      IF lv_is_update_requested = abap_true.
        " Edit of an existing record -> check update authorization
        IF lv_has_before_image = abap_true.
          lv_is_update_granted = is_update_granted( iv_has_before_image = lv_has_before_image  iv_status = travel_before_image-status ).
          IF lv_is_update_granted = abap_false.
            APPEND VALUE #( %tky        = travel-%tky
                            %msg        = NEW zcm_rap_ib( severity = if_abap_behv_message=>severity-error
                                                            textid   = zcm_rap_ib=>unauthorized )
                          ) TO reported-travel.
          ENDIF.
          " Creation of a new record -> check create authorization
        ELSE.
          lv_is_update_granted = is_create_granted( ).
          IF lv_is_update_granted = abap_false.
            APPEND VALUE #( %tky        = travel-%tky
                            %msg        = NEW zcm_rap_ib( severity = if_abap_behv_message=>severity-error
                                                            textid   = zcm_rap_ib=>unauthorized )
                          ) TO reported-travel.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lv_is_delete_requested = abap_true.
        lv_is_delete_granted = is_delete_granted( iv_has_before_image = lv_has_before_image  iv_status = travel_before_image-status ).
        IF lv_is_delete_granted = abap_false.
          APPEND VALUE #( %tky        = travel-%tky
                          %msg        = NEW zcm_rap_ib( severity = if_abap_behv_message=>severity-error
                                                          textid   = zcm_rap_ib=>unauthorized )
                        ) TO reported-travel.
        ENDIF.
      ENDIF.

      APPEND VALUE #( %tky = travel-%tky

                      %update              = COND #( WHEN lv_is_update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                      %action-acceptTravel = COND #( WHEN lv_is_update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                      %action-rejectTravel = COND #( WHEN lv_is_update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                      %action-Prepare      = COND #( WHEN lv_is_update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized ) "draft
                      %action-Edit         = COND #( WHEN lv_is_update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized ) "draft
                      %assoc-_Booking      = COND #( WHEN lv_is_update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )

                      %delete              = COND #( WHEN lv_is_delete_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                    )
        TO result.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_update_granted.
    IF iv_has_before_image = abap_true.
      AUTHORITY-CHECK OBJECT 'ZRAP_IB_ST'
        ID 'ZRAP_IB_ST' FIELD iv_status
        ID 'ACTVT' FIELD '02'.
    ELSE.
      AUTHORITY-CHECK OBJECT 'ZRAP_IB_ST'
        ID 'ZRAP_IB_ST' DUMMY
        ID 'ACTVT' FIELD '02'.
    ENDIF.
    rv_is_update_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    " Simulate full access - for testing purposes only! Needs to be removed for a productive implementation.
    rv_is_update_granted = abap_true.
  ENDMETHOD.

  METHOD is_delete_granted.
    IF iv_has_before_image = abap_true.
      AUTHORITY-CHECK OBJECT 'ZRAP_IB_ST'
        ID 'ZRAP_IB_ST' FIELD iv_status
        ID 'ACTVT' FIELD '06'.
    ELSE.
      AUTHORITY-CHECK OBJECT 'ZRAP_IB_ST'
        ID 'ZRAP_IB_ST' DUMMY
        ID 'ACTVT' FIELD '06'.
    ENDIF.
    rv_is_delete_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    " Simulate full access - for testing purposes only! Needs to be removed for a productive implementation.
    rv_is_delete_granted = abap_true.
  ENDMETHOD.

  METHOD is_create_granted.
    AUTHORITY-CHECK OBJECT 'ZRAP_IB_ST'
      ID 'ZRAP_IB_ST' DUMMY
      ID 'ACTVT' FIELD '01'.
    rv_is_create_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    " Simulate full access - for testing purposes only! Needs to be removed for a productive implementation.
    rv_is_create_granted = abap_true.
  ENDMETHOD.

ENDCLASS.
