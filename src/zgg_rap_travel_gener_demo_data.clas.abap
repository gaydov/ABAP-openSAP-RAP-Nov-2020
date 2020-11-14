CLASS zgg_rap_travel_gener_demo_data DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZGG_RAP_TRAVEL_GENER_DEMO_DATA IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    DELETE FROM zgg_rap_travel.
    DELETE FROM zgg_rap_booking.

    " Insert travel demo data
    INSERT zgg_rap_travel FROM (
    SELECT
    FROM /dmo/travel
    FIELDS
        uuid(  )      AS travel_uuid           ,
        travel_id     AS travel_id             ,
        agency_id     AS agency_id             ,
        customer_id   AS customer_id           ,
        begin_date    AS begin_date            ,
        end_date      AS end_date              ,
        booking_fee   AS booking_fee           ,
        total_price   AS total_price           ,
        currency_code AS currency_code         ,
        description   AS description           ,
        CASE status
          WHEN 'B' THEN 'A' " accepted
          WHEN 'X' THEN 'X' " cancelled
          ELSE 'O'          " open
        END           AS overall_status        ,
        createdby     AS created_by            ,
        createdat     AS created_at            ,
        lastchangedby AS last_changed_by       ,
        lastchangedat AS last_changed_at       ,
        lastchangedat AS local_last_changed_at
        ORDER BY travel_id UP TO 30 ROWS
  ).

    COMMIT WORK.

    " Insert booking demo data
    INSERT zgg_rap_booking FROM (
        SELECT
         FROM   /dmo/booking    AS booking
         JOIN zgg_rap_travel AS travel ON booking~travel_id = travel~travel_id
          FIELDS
            uuid( )                 AS booking_uuid          ,
            travel~travel_uuid      AS travel_uuid           ,
            booking~booking_id      AS booking_id            ,
            booking~booking_date    AS booking_date          ,
            booking~customer_id     AS customer_id           ,
            booking~carrier_id      AS carrier_id            ,
            booking~connection_id   AS connection_id         ,
            booking~flight_date     AS flight_date           ,
            booking~flight_price    AS flight_price          ,
            booking~currency_code   AS currency_code         ,
            travel~created_by       AS created_by            ,
            travel~last_changed_by  AS last_changed_by       ,
            travel~last_changed_at  AS local_last_changed_by
      ).

    COMMIT WORK.

    out->write( 'Travel and booking demo data inserted.').

  ENDMETHOD.
ENDCLASS.
