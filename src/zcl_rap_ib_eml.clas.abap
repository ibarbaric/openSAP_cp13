CLASS zcl_rap_ib_eml DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_RAP_IB_EML IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
**   step 1 - READ with no fields -> only key fields are read
*    READ ENTITIES OF zi_rap_ib_travel
*      ENTITY Travel
*        FROM VALUE #( ( TravelUUID = '70590E00131F0D4B17000B02CFCF0ECB' )  )
*        RESULT DATA(lt_travels).
*
**   step 2 - READ with fields
*    READ ENTITIES OF zi_rap_ib_travel
*      ENTITY Travel
*        FIELDS ( AgencyID CustomerID )
*        WITH VALUE #( ( TravelUUID = '70590E00131F0D4B17000B02CFCF0ECB' ) ) "WARNING! different key word WITH!
*        RESULT lt_travels.
*
**   step 3 - READ with ALL fields
*    READ ENTITIES OF zi_rap_ib_travel
*      ENTITY Travel
*        ALL FIELDS
*        WITH VALUE #( ( TravelUUID = '70590E00131F0D4B17000B02CFCF0ECB' ) ) "WARNING! different key word WITH!
*        RESULT lt_travels.
*
**   output Travels to console
*    out->write( lt_travels ).
*
**   step 4 - READ by association
*    READ ENTITIES OF zi_rap_ib_travel
*      ENTITY Travel BY \_Booking
*        ALL FIELDS
*        WITH VALUE #( ( TravelUUID = '70590E00131F0D4B17000B02CFCF0ECB' ) ) "WARNING! different key word WITH!
*        RESULT DATA(lt_bookings).
**    out->write( lt_bookings ).
*
**   step 5 - error handling
*    READ ENTITIES OF zi_rap_ib_travel
*      ENTITY Travel
*        ALL FIELDS
*        WITH VALUE #( ( TravelUUID = 'BLABLA' ) ) "WARNING! different key word WITH!
*        RESULT lt_travels              "empty
*        FAILED DATA(lt_travels_failed) "deep table: TRAVEL -> FAIL -> CAUSE = NOT FOUND
*        REPORTED DATA(ls_reported).    "empty struc in this case
*    out->write( lt_travels ).
*    out->write( lt_travels_failed ).
*    out->write( ls_reported ).

**   step 6 - MODIFY update
*    MODIFY ENTITIES OF zi_rap_ib_travel
*      ENTITY Travel
*      UPDATE
*        SET FIELDS WITH VALUE
*           #(  (  TravelUUID  = '70590E00131F0D4B17000B02CFCF0ECB'
*                  Description = 'Modified at ' && sy-datum && sy-uzeit ) )
*        FAILED DATA(lt_travels_failed)
*        REPORTED DATA(ls_reported).
*    out->write( 'Updated' ).
*    COMMIT ENTITIES
*      RESPONSE OF zi_rap_ib_travel
*      FAILED DATA(lt_travel_commit_failed)
*      REPORTED DATA(ls_travel_commit_reported).
*
**   step 7 - MODIFY Create
*    MODIFY ENTITIES OF zi_rap_ib_travel
*      ENTITY Travel
*      CREATE
*        SET FIELDS WITH VALUE
*           #(  (  %cid        = 'Content_ID_' && sy-datum && sy-uzeit
*                  AgencyID    = '70012'
*                  CustomerID  = '14'
*                  BeginDate   = cl_abap_context_info=>get_system_date( )
*                  EndDate     = cl_abap_context_info=>get_system_date( ) + 10
*                  Description = 'Modified at_' && sy-datum && sy-uzeit ) )
*        FAILED DATA(lt_travels_failed)
*        REPORTED DATA(ls_reported).
*    out->write( 'Created' ).
*    COMMIT ENTITIES
*      RESPONSE OF zi_rap_ib_travel
*      FAILED DATA(lt_travel_commit_failed)
*      REPORTED DATA(ls_travel_commit_reported).

*   step 8 - MODIFY Create
    MODIFY ENTITIES OF zi_rap_ib_travel
      ENTITY Travel
      DELETE FROM
        VALUE
           #(  (  TravelUUID = '02E8C215A2061EDB88CDB03CFE258FD4' ) )
        FAILED DATA(lt_travels_failed)
        REPORTED DATA(ls_reported).
    out->write( 'Deleted' ).
    COMMIT ENTITIES
      RESPONSE OF zi_rap_ib_travel
      FAILED DATA(lt_travel_commit_failed)
      REPORTED DATA(ls_travel_commit_reported).

  ENDMETHOD.
ENDCLASS.
