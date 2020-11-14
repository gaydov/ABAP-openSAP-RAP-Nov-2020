CLASS lhc_travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE travel.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE travel.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE travel.

    METHODS read FOR READ
      IMPORTING keys FOR READ travel RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK travel.

    METHODS rba_booking FOR READ
      IMPORTING keys_rba FOR READ travel\_booking FULL result_requested RESULT result LINK association_links.

    METHODS cba_booking FOR MODIFY
      IMPORTING entities_cba FOR CREATE travel\_booking.

ENDCLASS.

CLASS lhc_travel IMPLEMENTATION.

  METHOD create.

    DATA messages   TYPE /dmo/t_message.
    DATA legacy_entity_in  TYPE /dmo/travel.
    DATA legacy_entity_out TYPE /dmo/travel.
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

      legacy_entity_in = CORRESPONDING #( <entity> MAPPING FROM ENTITY USING CONTROL ).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_CREATE'
        EXPORTING
          is_travel   = CORRESPONDING /dmo/s_travel_in( legacy_entity_in )
        IMPORTING
          es_travel   = legacy_entity_out
          et_messages = messages.

      IF messages IS INITIAL.
        APPEND VALUE #( %cid = <entity>-%cid travelid = legacy_entity_out-travel_id ) TO mapped-travel.
      ELSE.

        "fill failed return structure for the framework
        APPEND VALUE #( travelid = legacy_entity_in-travel_id ) TO failed-travel.
        "fill reported structure to be displayed on the UI
        APPEND VALUE #( travelid = legacy_entity_in-travel_id
                        %msg = new_message( id = messages[ 1 ]-msgid
                                            number = messages[ 1 ]-msgno
                                            v1 = messages[ 1 ]-msgv1
                                            v2 = messages[ 1 ]-msgv2
                                            v3 = messages[ 1 ]-msgv3
                                            v4 = messages[ 1 ]-msgv4
                                            severity = CONV #( messages[ 1 ]-msgty ) )
       ) TO reported-travel.


      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD update.

    DATA legacy_entity_in   TYPE /dmo/travel.
    DATA legacy_entity_x  TYPE /dmo/s_travel_inx . "refers to x structure (> BAPIs)
    DATA messages TYPE /dmo/t_message.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<entity>).

      legacy_entity_in = CORRESPONDING #( <entity> MAPPING FROM ENTITY ).
      legacy_entity_x-travel_id = <entity>-travelid.
      legacy_entity_x-_intx = CORRESPONDING zggs_rap_travel( <entity> MAPPING FROM ENTITY ).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
        EXPORTING
          is_travel   = CORRESPONDING /dmo/s_travel_in( legacy_entity_in )
          is_travelx  = legacy_entity_x
        IMPORTING
          et_messages = messages.

      IF messages IS INITIAL.

        APPEND VALUE #( travelid = legacy_entity_in-travel_id ) TO mapped-travel.

      ELSE.

        "fill failed return structure for the framework
        APPEND VALUE #( travelid = legacy_entity_in-travel_id ) TO failed-travel.
        "fill reported structure to be displayed on the UI
        APPEND VALUE #( travelid = legacy_entity_in-travel_id
                        %msg = new_message( id = messages[ 1 ]-msgid
                                            number = messages[ 1 ]-msgno
                                            v1 = messages[ 1 ]-msgv1
                                            v2 = messages[ 1 ]-msgv2
                                            v3 = messages[ 1 ]-msgv3
                                            v4 = messages[ 1 ]-msgv4
                                            severity = CONV #( messages[ 1 ]-msgty ) )
       ) TO reported-travel.

      ENDIF.


    ENDLOOP.

  ENDMETHOD.

  METHOD delete.

    DATA messages TYPE /dmo/t_message.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<key>).

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_DELETE'
        EXPORTING
          iv_travel_id = <key>-travelid
        IMPORTING
          et_messages  = messages.

      IF messages IS INITIAL.

        APPEND VALUE #( travelid = <key>-travelid ) TO mapped-travel.

      ELSE.

        "fill failed return structure for the framework
        APPEND VALUE #( travelid = <key>-travelid ) TO failed-travel.
        "fill reported structure to be displayed on the UI
        APPEND VALUE #( travelid = <key>-travelid
                        %msg = new_message( id = messages[ 1 ]-msgid
                                            number = messages[ 1 ]-msgno
                                            v1 = messages[ 1 ]-msgv1
                                            v2 = messages[ 1 ]-msgv2
                                            v3 = messages[ 1 ]-msgv3
                                            v4 = messages[ 1 ]-msgv4
                                            severity = CONV #( messages[ 1 ]-msgty ) )
       ) TO reported-travel.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD read.

    DATA: legacy_entity_out TYPE /dmo/travel,
          messages          TYPE /dmo/t_message.

    LOOP AT keys INTO DATA(key) GROUP BY key-travelid.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = key-travelid
        IMPORTING
          es_travel    = legacy_entity_out
          et_messages  = messages.

      IF messages IS INITIAL.
        "fill result parameter with flagged fields

        INSERT CORRESPONDING #( legacy_entity_out MAPPING TO ENTITY ) INTO TABLE result.

      ELSE.

        "fill failed return structure for the framework
        APPEND VALUE #( travelid = key-travelid ) TO failed-travel.

        LOOP AT messages INTO DATA(message).

          "fill reported structure to be displayed on the UI
          APPEND VALUE #( travelid = key-travelid
                          %msg = new_message( id = message-msgid
                                              number = message-msgno
                                              v1 = message-msgv1
                                              v2 = message-msgv2
                                              v3 = message-msgv3
                                              v4 = message-msgv4
                                              severity = CONV #( message-msgty ) )


         ) TO reported-travel.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD lock.

    "Instantiate lock object
    DATA(lock) = cl_abap_lock_object_factory=>get_instance( iv_name = '/DMO/ETRAVEL' ).


    LOOP AT keys ASSIGNING FIELD-SYMBOL(<key>).
      TRY.
          "enqueue travel instance
          lock->enqueue(
              it_parameter  = VALUE #( (  name = 'TRAVEL_ID' value = REF #( <key>-travelid ) ) )
          ).
          "if foreign lock exists
        CATCH cx_abap_foreign_lock INTO DATA(lx_foreign_lock).

          "fill failed return structure for the framework
          APPEND VALUE #( travelid = <key>-travelid ) TO failed-travel.
          "fill reported structure to be displayed on the UI
          APPEND VALUE #( travelid = <key>-travelid
                          %msg = new_message( id = '/DMO/CM_FLIGHT_LEGAC'
                                              number = '032'
                                              v1 = <key>-travelid
                                              v2 = lx_foreign_lock->user_name
                                              severity = CONV #( 'E' ) )
         ) TO reported-travel.

      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD rba_booking.

    DATA: legacy_parent_entity_out TYPE /dmo/travel,
          legacy_entities_out      TYPE /dmo/t_booking,
          entity                   LIKE LINE OF result,
          message                  TYPE /dmo/t_message.


    LOOP AT keys_rba  ASSIGNING FIELD-SYMBOL(<key_rba>) GROUP BY <key_rba>-travelid.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = <key_rba>-travelid
        IMPORTING
          es_travel    = legacy_parent_entity_out
          et_booking   = legacy_entities_out
          et_messages  = message.

      IF message IS INITIAL.

        LOOP AT legacy_entities_out ASSIGNING FIELD-SYMBOL(<fs_booking>).
          "fill link table with key fields

          INSERT
            VALUE #(
                source-%key = <key_rba>-%key
                target-%key = VALUE #(
                    travelid  = <fs_booking>-travel_id
                    bookingid = <fs_booking>-booking_id
                )
            )
            INTO TABLE association_links.

          "fill result parameter with flagged fields
          IF result_requested = abap_true.

            entity = CORRESPONDING #( <fs_booking> MAPPING TO ENTITY ).
            INSERT entity INTO TABLE result.

          ENDIF.

        ENDLOOP.

      ELSE.
        "fill failed table in case of error

        failed-travel = VALUE #(
          BASE failed-travel
          FOR msg IN message (
            %key = <key_rba>-travelid
            %fail-cause = COND #(
              WHEN msg-msgty = 'E' AND  ( msg-msgno = '016' OR msg-msgno = '009' )
              THEN if_abap_behv=>cause-not_found
              ELSE if_abap_behv=>cause-unspecific
            )
          )
        ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD cba_booking.

    DATA messages        TYPE /dmo/t_message.
    DATA lt_booking_old     TYPE /dmo/t_booking.
    DATA entity         TYPE /dmo/booking.
    DATA last_booking_id TYPE /dmo/booking_id VALUE '0'.

    LOOP AT entities_cba ASSIGNING FIELD-SYMBOL(<entity_cba>).

      DATA(travelid) = <entity_cba>-travelid.

      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
        EXPORTING
          iv_travel_id = travelid
        IMPORTING
          et_booking   = lt_booking_old
          et_messages  = messages.

      IF messages IS INITIAL.

        IF lt_booking_old IS NOT INITIAL.

          last_booking_id = lt_booking_old[ lines( lt_booking_old ) ]-booking_id.

        ENDIF.

        LOOP AT <entity_cba>-%target ASSIGNING FIELD-SYMBOL(<entity>).

          entity = CORRESPONDING #( <entity> MAPPING FROM ENTITY USING CONTROL ) .

          last_booking_id += 1.
          entity-booking_id = last_booking_id.

          CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
            EXPORTING
              is_travel   = VALUE /dmo/s_travel_in( travel_id = travelid )
              is_travelx  = VALUE /dmo/s_travel_inx( travel_id = travelid )
              it_booking  = VALUE /dmo/t_booking_in( ( CORRESPONDING #( entity ) ) )
              it_bookingx = VALUE /dmo/t_booking_inx(
                (
                  booking_id  = entity-booking_id
                  action_code = /dmo/if_flight_legacy=>action_code-create
                )
              )
            IMPORTING
              et_messages = messages.

          IF messages IS INITIAL.

            INSERT
              VALUE #(
                %cid = <entity>-%cid
                travelid = travelid
                bookingid = entity-booking_id
              )
              INTO TABLE mapped-booking.

          ELSE.


            INSERT VALUE #( %cid = <entity>-%cid travelid = travelid ) INTO TABLE failed-booking.

            LOOP AT messages INTO DATA(message) WHERE msgty = 'E' OR msgty = 'A'.

              INSERT
                 VALUE #(
                   %cid     = <entity>-%cid
                   travelid = <entity>-travelid
                   %msg     = new_message(
                     id       = message-msgid
                     number   = message-msgno
                     severity = if_abap_behv_message=>severity-error
                     v1       = message-msgv1
                     v2       = message-msgv2
                     v3       = message-msgv3
                     v4       = message-msgv4
                   )
                 )
                 INTO TABLE reported-booking.

            ENDLOOP.

          ENDIF.

        ENDLOOP.

      ELSE.

        "fill failed return structure for the framework
        APPEND VALUE #( travelid = travelid ) TO failed-travel.
        "fill reported structure to be displayed on the UI
        APPEND VALUE #( travelid = travelid
                        %msg = new_message( id = messages[ 1 ]-msgid
                                            number = messages[ 1 ]-msgno
                                            v1 = messages[ 1 ]-msgv1
                                            v2 = messages[ 1 ]-msgv2
                                            v3 = messages[ 1 ]-msgv3
                                            v4 = messages[ 1 ]-msgv4
                                            severity = CONV #( messages[ 1 ]-msgty ) )
       ) TO reported-travel.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lsc_zggi_rap_u_travel DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

ENDCLASS.

CLASS lsc_zggi_rap_u_travel IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.

    CALL FUNCTION '/DMO/FLIGHT_TRAVEL_SAVE'.

  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

ENDCLASS.
