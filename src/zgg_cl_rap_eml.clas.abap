CLASS zgg_cl_rap_eml DEFINITION
  PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    CONSTANTS travel_uuid TYPE sysuuid_x16 VALUE '8DD71800131F0D4B17000B02CFCF0ECB'.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZGG_CL_RAP_EML IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    " 1 - READ
*    READ ENTITIES OF zggi_rap_travel
*        ENTITY travel
*            FROM VALUE #( ( traveluuid = travel_uuid ) )
*        RESULT DATA(travels).
*
*    out->write( travels ).

*    " 2 READ with FIELDS
*    READ ENTITIES OF zggi_rap_travel
*        ENTITY travel
*            FIELDS ( agencyid customerid )
*        WITH VALUE #( ( traveluuid = travel_uuid ) )
*        RESULT DATA(travels).
*
*    out->write( travels ).

    " 3 READ all fields
*    READ ENTITIES OF zggi_rap_travel
*        ENTITY travel
*            ALL FIELDS
*        WITH VALUE #( ( traveluuid = travel_uuid ) )
*        RESULT DATA(travels).
*
*    out->write( travels ).

*    " 4 READ By Association
*    READ ENTITIES OF zggi_rap_travel
*      ENTITY travel BY \_booking
*        ALL FIELDS WITH VALUE #( ( traveluuid = travel_uuid ) )
*      RESULT DATA(bookings).
*
*    out->write( bookings ).

    " 5 Unsuccessful READ
*    READ ENTITIES OF zggi_rap_travel
*        ENTITY travel
*            ALL FIELDS WITH VALUE #( ( traveluuid = '9999' ) )
*        RESULT DATA(travels)
*        FAILED DATA(failed)
*        REPORTED DATA(reported).
*
*    out->write( travels ).
*    out->write( failed ).
*    out->write( reported ).

    " 6 MODIFY update
*    MODIFY ENTITIES OF zggi_rap_travel
*        ENTITY travel
*            UPDATE
*                SET FIELDS WITH VALUE #( ( traveluuid = travel_uuid
*                                           description = 'Gaydov changed description' ) )
*        FAILED DATA(failed)
*        REPORTED DATA(reported).
*
*    COMMIT ENTITIES
*        RESPONSE OF zggi_rap_travel
*        FAILED DATA(failed_commit)
*        REPORTED DATA(reported_commit).
*
*    out->write( 'Update completed.' ).

    " 7 MODIFY create
*    MODIFY ENTITIES OF zggi_rap_travel
*        ENTITY travel
*            CREATE
*                SET FIELDS WITH VALUE #(
*                   ( %cid        = 'MyContentID_1' " temporary key because the record is not persisted yet
*                     agencyid    = '70012'
*                     customerid  = '14'
*                     begindate   = cl_abap_context_info=>get_system_date( )
*                     enddate     = cl_abap_context_info=>get_system_date( ) + 10
*                     description = 'Gaydov RAP' )
*                )
*             MAPPED DATA(mapped)
*             FAILED DATA(failed)
*             REPORTED DATA(reported).
*
*    out->write( mapped-travel ).
*
*    COMMIT ENTITIES
*      RESPONSE OF zggi_rap_travel
*      FAILED     DATA(failed_commit)
*      REPORTED   DATA(reported_commit).
*
*    out->write( 'Create done' ).

    " 8 MODIFY delete
*    MODIFY ENTITIES OF zggi_rap_travel
*        ENTITY travel
*            DELETE FROM VALUE #( ( traveluuid = '02ED5684F31E1EDB88A58F12750A0EB4' ) ).
*
*    COMMIT ENTITIES
*      RESPONSE OF zggi_rap_travel
*      FAILED     DATA(failed_commit)
*      REPORTED   DATA(reported_commit).
*
*    out->write( 'Delete done' ).

  ENDMETHOD.
ENDCLASS.
