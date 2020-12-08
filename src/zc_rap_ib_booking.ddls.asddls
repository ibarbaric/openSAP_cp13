@EndUserText.label: 'Bookings - projection view'
@AccessControl.authorizationCheck: #CHECK
@Search.searchable: true
@Metadata.allowExtensions: true
define view entity ZC_RAP_IB_BOOKING 
as projection on ZI_RAP_IB_Booking as Booking {
    //ZI_RAP_IB_BOOKING
    key BookingUUID,
    TravelUUID,
    @Search.defaultSearchElement: true
    
    BookingID,
    BookingDate,
    @Consumption.valueHelpDefinition: [{ entity : {name: '/DMO/I_Customer', element: 'CustomerID'  } }]
    @Search.defaultSearchElement: true
    @ObjectModel.text.element: ['CustomerName']
    CustomerID,
    _Customer.LastName as CustomerName,
    @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Carrier', element: 'AirlineID' }}]
    @ObjectModel.text.element: ['CarrierName']
    CarrierID,
    _Carrier.Name as CarrierName,
     @Consumption.valueHelpDefinition: [ {entity: {name: '/DMO/I_Flight', element: 'ConnectionID'},
                                           additionalBinding: [ { localElement: 'CarrierID',    element: 'AirlineID' },
                                                                { localElement: 'FlightDate',   element: 'FlightDate',   usage: #RESULT},
                                                                { localElement: 'FlightPrice',  element: 'Price',        usage: #RESULT },
                                                                { localElement: 'CurrencyCode', element: 'CurrencyCode', usage: #RESULT } ] } ]
    ConnectionID,
    FlightDate,
    @Semantics.amount.currencyCode: 'CurrencyCode'
    FlightPrice,
    @Consumption.valueHelpDefinition: [{entity: {name: 'I_Currency', element: 'Currency' }}]
    CurrencyCode,
    CreatedBy,
    LastChangedBy,
    LocalLastChangedAt,
    /* Associations */
    //ZI_RAP_IB_BOOKING
    _Travel: redirected to parent ZC_RAP_IB_TRAVEL, 
    _Carrier,
    _Connection,
    _Currency,
    _Customer,
    _Flight
}
