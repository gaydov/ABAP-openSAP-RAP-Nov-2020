CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF travel_status,
        open     TYPE c LENGTH 1  VALUE 'O', " Open
        accepted TYPE c LENGTH 1  VALUE 'A', " Accepted
        canceled TYPE c LENGTH 1  VALUE 'X', " Cancelled
      END OF travel_status.

    METHODS calculatetotalprice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR travel~calculatetotalprice.

    METHODS calculatetravelid FOR DETERMINE ON SAVE
      IMPORTING keys FOR travel~calculatetravelid.

    METHODS setinitialstatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR travel~setinitialstatus.

    METHODS validateagency FOR VALIDATE ON SAVE
      IMPORTING keys FOR travel~validateagency.

    METHODS validatecustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR travel~validatecustomer.

    METHODS validatedates FOR VALIDATE ON SAVE
      IMPORTING keys FOR travel~validatedates.

    METHODS accepttravel FOR MODIFY
      IMPORTING keys FOR ACTION travel~accepttravel RESULT result.

    METHODS rejecttravel FOR MODIFY
      IMPORTING keys FOR ACTION travel~rejecttravel RESULT result.

    METHODS recalctotalprice FOR MODIFY
      IMPORTING keys FOR ACTION travel~recalctotalprice.

    METHODS get_features FOR FEATURES
      IMPORTING keys REQUEST requested_features FOR travel RESULT result.

    METHODS get_authorizations FOR AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR travel RESULT result.

    METHODS is_update_granted IMPORTING has_before_image      TYPE abap_bool
                                        overall_status        TYPE /dmo/overall_status
                              RETURNING VALUE(update_granted) TYPE abap_bool.

    METHODS is_delete_granted IMPORTING has_before_image      TYPE abap_bool
                                        overall_status        TYPE /dmo/overall_status
                              RETURNING VALUE(delete_granted) TYPE abap_bool.

    METHODS is_create_granted RETURNING VALUE(create_granted) TYPE abap_bool.

ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

  METHOD calculatetotalprice.

    MODIFY ENTITIES OF zggi_rap_travel IN LOCAL MODE
      ENTITY travel
        EXECUTE recalctotalprice
        FROM CORRESPONDING #( keys )
      REPORTED DATA(execute_reported).

    reported = CORRESPONDING #( DEEP execute_reported ).

  ENDMETHOD.

  METHOD calculatetravelid.
    " Please note that this is just an example for calculating a field during _onSave_.
    " This approach does NOT ensure for gap free or unique travel IDs! It just helps to provide a readable ID.
    " The key of this business object is a UUID, calculated by the framework.

    " check if TravelID is already filled
    READ ENTITIES OF zggi_rap_travel IN LOCAL MODE
      ENTITY travel
        FIELDS ( travelid ) WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    " remove lines where TravelID is already filled.
    DELETE travels WHERE travelid IS NOT INITIAL.

    IF travels IS NOT INITIAL.

      " Select max travel ID
      SELECT SINGLE
      FROM  zgg_rap_travel
      FIELDS MAX( travel_id ) AS travelid
      INTO @DATA(max_travelid).

      " Set the travel ID
      MODIFY ENTITIES OF zggi_rap_travel IN LOCAL MODE
      ENTITY travel
        UPDATE
          FROM VALUE #( FOR travel IN travels INDEX INTO i (
            %tky              = travel-%tky
            travelid          = max_travelid + i
            %control-travelid = if_abap_behv=>mk-on ) )
      REPORTED DATA(update_reported).

      reported = CORRESPONDING #( DEEP update_reported ).
    ENDIF.
  ENDMETHOD.

  METHOD setinitialstatus.
    " Read relevant travel instance data
    READ ENTITIES OF zggi_rap_travel IN LOCAL MODE
      ENTITY travel
        FIELDS ( travelstatus ) WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    " Remove all travel instance data with defined status
    DELETE travels WHERE travelstatus IS NOT INITIAL.

    IF travels IS NOT INITIAL.
      " Set default travel status
      MODIFY ENTITIES OF zggi_rap_travel IN LOCAL MODE
      ENTITY travel
        UPDATE
          FIELDS ( travelstatus )
          WITH VALUE #( FOR travel IN travels
                        ( %tky         = travel-%tky
                          travelstatus = travel_status-open ) )
      REPORTED DATA(update_reported).

      reported = CORRESPONDING #( DEEP update_reported ).
    ENDIF.
  ENDMETHOD.

  METHOD validateagency.
    " Read relevant travel instance data
    READ ENTITIES OF zggi_rap_travel IN LOCAL MODE
      ENTITY travel
        FIELDS ( agencyid ) WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    DATA agencies TYPE SORTED TABLE OF /dmo/agency WITH UNIQUE KEY agency_id.

    " Optimization of DB select: extract distinct non-initial agency IDs
    agencies = CORRESPONDING #( travels DISCARDING DUPLICATES MAPPING agency_id = agencyid EXCEPT * ).
    DELETE agencies WHERE agency_id IS INITIAL.

    IF agencies IS NOT INITIAL.
      " Check if agency ID exist
      SELECT FROM /dmo/agency FIELDS agency_id
        FOR ALL ENTRIES IN @agencies
        WHERE agency_id = @agencies-agency_id
        INTO TABLE @DATA(agencies_db).
    ENDIF.

    " Raise msg for non existing and initial agencyID
    LOOP AT travels INTO DATA(travel).
      " Clear state messages that might exist
      APPEND VALUE #(  %tky               = travel-%tky
                       %state_area        = 'VALIDATE_AGENCY' )
        TO reported-travel.

      IF travel-agencyid IS INITIAL OR NOT line_exists( agencies_db[ agency_id = travel-agencyid ] ).
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #( %tky        = travel-%tky
                        %state_area = 'VALIDATE_AGENCY'
                        %msg        = NEW zggcx_rap(
                                          severity = if_abap_behv_message=>severity-error
                                          textid   = zggcx_rap=>agency_unknown
                                          agencyid = travel-agencyid )
                        %element-agencyid = if_abap_behv=>mk-on ) " The reported table includes the component %element. Here you can specify which CDS 440PUBLICSAP - ABAP RESTful Application Programming ModelDevelop
                                                                  " element is responsible for the state inconsistency.
          TO reported-travel.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD validatecustomer.
    " Read relevant travel instance data
    READ ENTITIES OF zggi_rap_travel IN LOCAL MODE
      ENTITY travel
        FIELDS ( customerid ) WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    DATA customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    " Optimization of DB select: extract distinct non-initial customer IDs
    customers = CORRESPONDING #( travels DISCARDING DUPLICATES MAPPING customer_id = customerid EXCEPT * ).
    DELETE customers WHERE customer_id IS INITIAL.
    IF customers IS NOT INITIAL.
      " Check if customer ID exist
      SELECT FROM /dmo/customer FIELDS customer_id
        FOR ALL ENTRIES IN @customers
        WHERE customer_id = @customers-customer_id
        INTO TABLE @DATA(customers_db).
    ENDIF.

    " Raise msg for non existing and initial customerID
    LOOP AT travels INTO DATA(travel).
      " Clear state messages that might exist
      APPEND VALUE #(  %tky        = travel-%tky
                       %state_area = 'VALIDATE_CUSTOMER' )
        TO reported-travel.

      IF travel-customerid IS INITIAL OR NOT line_exists( customers_db[ customer_id = travel-customerid ] ).
        APPEND VALUE #(  %tky = travel-%tky ) TO failed-travel.

        APPEND VALUE #(  %tky        = travel-%tky
                         %state_area = 'VALIDATE_CUSTOMER'
                         %msg        = NEW zggcx_rap(
                                           severity   = if_abap_behv_message=>severity-error
                                           textid     = zggcx_rap=>customer_unknown
                                           customerid = travel-customerid )
                         %element-customerid = if_abap_behv=>mk-on )
          TO reported-travel.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD validatedates.
    " Read relevant travel instance data
    READ ENTITIES OF zggi_rap_travel IN LOCAL MODE
      ENTITY travel
        FIELDS ( travelid begindate enddate ) WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    LOOP AT travels INTO DATA(travel).
      " Clear state messages that might exist
      APPEND VALUE #(  %tky        = travel-%tky
                       %state_area = 'VALIDATE_DATES' )
        TO reported-travel.

      IF travel-enddate < travel-begindate.
        APPEND VALUE #( %tky = travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                        %msg               = NEW zggcx_rap(
                                                 severity  = if_abap_behv_message=>severity-error
                                                 textid    = zggcx_rap=>date_interval
                                                 begindate = travel-begindate
                                                 enddate   = travel-enddate
                                                 travelid  = travel-travelid )
                        %element-begindate = if_abap_behv=>mk-on
                        %element-enddate   = if_abap_behv=>mk-on ) TO reported-travel.

      ELSEIF travel-begindate < cl_abap_context_info=>get_system_date( ).
        APPEND VALUE #( %tky               = travel-%tky ) TO failed-travel.
        APPEND VALUE #( %tky               = travel-%tky
                        %state_area        = 'VALIDATE_DATES'
                        %msg               = NEW zggcx_rap(
                                                 severity  = if_abap_behv_message=>severity-error
                                                 textid    = zggcx_rap=>begin_date_before_system_date
                                                 begindate = travel-begindate )
                        %element-begindate = if_abap_behv=>mk-on ) TO reported-travel.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD accepttravel.
    " Set the new overall status
    MODIFY ENTITIES OF zggi_rap_travel IN LOCAL MODE "
      ENTITY travel
         UPDATE
           FIELDS ( travelstatus )
           WITH VALUE #( FOR key IN keys
                           ( %tky         = key-%tky
                             travelstatus = travel_status-accepted ) )
      FAILED failed
      REPORTED reported.

    " The addition IN LOCAL MODE allows us to manipulate fields – e.g. to even change read-only fields
    " while skipping feature and authorization control.

    " Fill the response table
    READ ENTITIES OF zggi_rap_travel IN LOCAL MODE
      ENTITY travel
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    result = VALUE #( FOR travel IN travels
                        ( %tky   = travel-%tky
                          %param = travel ) ).
  ENDMETHOD.

  METHOD rejecttravel.
    " Set the new overall status
    MODIFY ENTITIES OF zggi_rap_travel IN LOCAL MODE
      ENTITY travel
         UPDATE
           FIELDS ( travelstatus )
           WITH VALUE #( FOR key IN keys
                           ( %tky         = key-%tky
                             travelstatus = travel_status-canceled ) )
      FAILED failed
      REPORTED reported.

    " Fill the response table
    READ ENTITIES OF zggi_rap_travel IN LOCAL MODE
      ENTITY travel
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(travels).

    result = VALUE #( FOR travel IN travels
                        ( %tky   = travel-%tky
                          %param = travel ) ).
  ENDMETHOD.

  METHOD recalctotalprice.

    TYPES: BEGIN OF ty_amount_per_currencycode,
             amount        TYPE /dmo/total_price,
             currency_code TYPE /dmo/currency_code,
           END OF ty_amount_per_currencycode.

    DATA: amount_per_currencycode TYPE STANDARD TABLE OF ty_amount_per_currencycode.

    " Read all relevant travel instances.
    READ ENTITIES OF zggi_rap_travel IN LOCAL MODE
          ENTITY travel
             FIELDS ( bookingfee currencycode )
             WITH CORRESPONDING #( keys )
          RESULT DATA(travels).

    DELETE travels WHERE currencycode IS INITIAL.

    LOOP AT travels ASSIGNING FIELD-SYMBOL(<travel>).
      " Set the start for the calculation by adding the booking fee.
      amount_per_currencycode = VALUE #( ( amount        = <travel>-bookingfee
                                           currency_code = <travel>-currencycode ) ).
      " Read all associated bookings and add them to the total price.
      READ ENTITIES OF zggi_rap_travel IN LOCAL MODE
         ENTITY travel BY \_booking
            FIELDS ( flightprice currencycode )
          WITH VALUE #( ( %tky = <travel>-%tky ) )
          RESULT DATA(bookings).

      LOOP AT bookings INTO DATA(booking) WHERE currencycode IS NOT INITIAL.
        COLLECT VALUE ty_amount_per_currencycode( amount        = booking-flightprice
                                                  currency_code = booking-currencycode ) INTO amount_per_currencycode.
      ENDLOOP.

      CLEAR <travel>-totalprice.
      LOOP AT amount_per_currencycode INTO DATA(single_amount_per_currencycode).
        " If needed do a Currency Conversion
        IF single_amount_per_currencycode-currency_code = <travel>-currencycode.
          <travel>-totalprice += single_amount_per_currencycode-amount.
        ELSE.
          /dmo/cl_flight_amdp=>convert_currency(
             EXPORTING
               iv_amount                   =  single_amount_per_currencycode-amount
               iv_currency_code_source     =  single_amount_per_currencycode-currency_code
               iv_currency_code_target     =  <travel>-currencycode
               iv_exchange_rate_date       =  cl_abap_context_info=>get_system_date( )
             IMPORTING
               ev_amount                   = DATA(total_booking_price_per_curr)
            ).

          <travel>-totalprice += total_booking_price_per_curr.

        ENDIF.
      ENDLOOP.
    ENDLOOP.

    " write back the modified total_price of travels
    MODIFY ENTITIES OF zggi_rap_travel IN LOCAL MODE
      ENTITY travel
        UPDATE FIELDS ( totalprice )
        WITH CORRESPONDING #( travels ).

  ENDMETHOD.

  METHOD get_features.
    " Read the travel status of the existing travels
    READ ENTITIES OF zggi_rap_travel IN LOCAL MODE
      ENTITY travel
        FIELDS ( travelstatus ) WITH CORRESPONDING #( keys )
      RESULT DATA(travels)
      FAILED failed.

    result =
      VALUE #(
        FOR travel IN travels
          LET is_accepted =   COND #( WHEN travel-travelstatus = travel_status-accepted
                                      THEN if_abap_behv=>fc-o-disabled
                                      ELSE if_abap_behv=>fc-o-enabled  )
              is_rejected =   COND #( WHEN travel-travelstatus = travel_status-canceled
                                      THEN if_abap_behv=>fc-o-disabled
                                      ELSE if_abap_behv=>fc-o-enabled )
          IN
            ( %tky                 = travel-%tky
              %action-accepttravel = is_accepted
              %action-rejecttravel = is_rejected
             ) ).
  ENDMETHOD.

  METHOD get_authorizations.

    DATA: has_before_image    TYPE abap_bool,
          is_update_requested TYPE abap_bool,
          is_delete_requested TYPE abap_bool,
          update_granted      TYPE abap_bool,
          delete_granted      TYPE abap_bool.

    DATA: failed_travel LIKE LINE OF failed-travel.

    " Read the existing travels
    READ ENTITIES OF zggi_rap_travel IN LOCAL MODE
      ENTITY travel
        FIELDS ( travelstatus ) WITH CORRESPONDING #( keys )
      RESULT DATA(travels)
      FAILED failed.

    CHECK travels IS NOT INITIAL.

    "   In this example the authorization is defined based on the Activity + Travel Status
    "   For the Travel Status we need the before-image from the database. We perform this for active (is_draft=00) as well as for drafts (is_draft=01) as we can't distinguish between edit or new drafts
    SELECT FROM zgg_rap_travel
      FIELDS travel_uuid, overall_status
      FOR ALL ENTRIES IN @travels
      WHERE travel_uuid EQ @travels-traveluuid
      ORDER BY PRIMARY KEY
      INTO TABLE @DATA(travels_before_image).

    is_update_requested = COND #( WHEN requested_authorizations-%update              = if_abap_behv=>mk-on OR
                                       requested_authorizations-%action-accepttravel = if_abap_behv=>mk-on OR
                                       requested_authorizations-%action-rejecttravel = if_abap_behv=>mk-on OR
                                       requested_authorizations-%action-prepare      = if_abap_behv=>mk-on OR
                                       requested_authorizations-%action-edit         = if_abap_behv=>mk-on OR
                                       requested_authorizations-%assoc-_booking      = if_abap_behv=>mk-on
                                  THEN abap_true ELSE abap_false ).

    is_delete_requested = COND #( WHEN requested_authorizations-%delete = if_abap_behv=>mk-on
                                  THEN abap_true ELSE abap_false ).

    LOOP AT travels INTO DATA(travel).
      update_granted = delete_granted = abap_false.

      READ TABLE travels_before_image INTO DATA(travel_before_image)
           WITH KEY travel_uuid = travel-traveluuid BINARY SEARCH.

      has_before_image = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

      IF is_update_requested = abap_true.
        " Edit of an existing record -> check update authorization
        IF has_before_image = abap_true.

          update_granted = is_update_granted( has_before_image = has_before_image  overall_status = travel_before_image-overall_status ).

          IF update_granted = abap_false.
            APPEND VALUE #( %tky        = travel-%tky
                            %msg        = NEW zggcx_rap( severity = if_abap_behv_message=>severity-error
                                                         textid   = zggcx_rap=>unauthorized )
                          ) TO reported-travel.
          ENDIF.
          " Creation of a new record -> check create authorization
        ELSE.
          update_granted = is_create_granted( ).
          IF update_granted = abap_false.
            APPEND VALUE #( %tky        = travel-%tky
                            %msg        = NEW zggcx_rap( severity = if_abap_behv_message=>severity-error
                                                         textid   = zggcx_rap=>unauthorized )
                          ) TO reported-travel.
          ENDIF.
        ENDIF.
      ENDIF.

      IF is_delete_requested = abap_true.
        delete_granted = is_delete_granted( has_before_image = has_before_image  overall_status = travel_before_image-overall_status ).
        IF delete_granted = abap_false.
          APPEND VALUE #( %tky        = travel-%tky
                          %msg        = NEW zggcx_rap( severity = if_abap_behv_message=>severity-error
                                                       textid   = zggcx_rap=>unauthorized )
                        ) TO reported-travel.
        ENDIF.
      ENDIF.

      APPEND VALUE #( %tky = travel-%tky

                      %update              = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                      %action-accepttravel = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                      %action-rejecttravel = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                      %action-prepare      = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                      %action-edit         = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                      %assoc-_booking      = COND #( WHEN update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )

                      %delete              = COND #( WHEN delete_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                    )
        TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD is_create_granted.
    AUTHORITY-CHECK OBJECT 'ZGG_TRVSTS'
      ID 'ZGG_TRVSTS' DUMMY
      ID 'ACTVT' FIELD '01'.
    create_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    " Simulate full access - for testing purposes only! Needs to be removed for a productive implementation.
    create_granted = abap_true.
  ENDMETHOD.

  METHOD is_delete_granted.

    IF has_before_image = abap_true.
      AUTHORITY-CHECK OBJECT 'ZGG_TRVSTS'
        ID 'ZGG_TRVSTS' FIELD overall_status
        ID 'ACTVT' FIELD '06'.
    ELSE.
      AUTHORITY-CHECK OBJECT 'ZGG_TRVSTS'
        ID 'ZGG_TRVSTS' DUMMY
        ID 'ACTVT' FIELD '06'.
    ENDIF.

    delete_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    " Simulate full access - for testing purposes only! Needs to be removed for a productive implementation.
    delete_granted = abap_true.

  ENDMETHOD.

  METHOD is_update_granted.

    IF has_before_image = abap_true.
      AUTHORITY-CHECK OBJECT 'ZGG_TRVSTS'
        ID 'ZGG_TRVSTS' FIELD overall_status
        ID 'ACTVT' FIELD '02'.
    ELSE.
      AUTHORITY-CHECK OBJECT 'ZGG_TRVSTS'
        ID 'ZGG_TRVSTS' DUMMY
        ID 'ACTVT' FIELD '02'.
    ENDIF.

    update_granted = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

    " Simulate full access - for testing purposes only! Needs to be removed for a productive implementation.
    update_granted = abap_true.
  ENDMETHOD.

ENDCLASS.
