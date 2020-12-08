@EndUserText.label: 'Travel - projection view'
@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@Search.searchable: true
define root view entity ZC_RAP_IB_TRAVEL as projection on ZI_RAP_IB_TRAVEL as Travel {
    //ZI_RAP_IB_TRAVEL
    key TravelUUID,
    @Search.defaultSearchElement: true
    TravelID,
    @Consumption.valueHelpDefinition: [{ entity: { name: 'ZCE_IB_RAP_AGENCY', element: 'AgencyId' } }] 
    @Search.defaultSearchElement: true
//    @ObjectModel.text.element: ['AgencyName']  //'custom entities must not be part of a projection' (?!)
    AgencyID,
//    _Agency.Name as AgencyName, //'custom entities must not be part of a projection' (?!)
    @Consumption.valueHelpDefinition: [{ entity: {name:'/DMO/I_Customer', element: 'CustomerID' } }]
    @Search.defaultSearchElement: true
    @ObjectModel.text.element: ['CustomerName']
    CustomerID,
    _Customer.LastName as CustomerName,
    BeginDate,
    EndDate,
    @Semantics.amount.currencyCode: 'CurrencyCode'
    BookingFee,
    @Semantics.amount.currencyCode: 'CurrencyCode'
    TotalPrice,
    @Consumption.valueHelpDefinition: [{ entity: {name:'I_Currency', element: 'Currency' } }]
    CurrencyCode,
    TravelStatus,
    Description,
    CreatedBy,
    CreatedAt,
    LastChangedBy,
    LastChangedAt,
    LocalLastChangedAt,
    /* Associations */
    //ZI_RAP_IB_TRAVEL
    _Agency,
    _Booking: redirected to composition child ZC_RAP_IB_BOOKING,
    _Currency,
    _Customer
}
