@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Travel view'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZI_RAP_IB_TRAVEL
as select from zrap_ib_atrav as Travel

  composition [0..*] of ZI_RAP_IB_Booking as _Booking

  association [0..1] to zce_ib_rap_agency as _Agency    on $projection.AgencyID     = _Agency.AgencyId // /DMO/I_Agency      as _Agency   on $projection.AgencyID = _Agency.AgencyID
  association [0..1] to /DMO/I_Customer    as _Customer on $projection.CustomerID   = _Customer.CustomerID
  association [0..1] to I_Currency         as _Currency on $projection.CurrencyCode = _Currency.Currency
{
  key travel_uuid           as TravelUUID,
      travel_id             as TravelID,
      agency_id             as AgencyID,
      customer_id           as CustomerID,
      begin_date            as BeginDate,
      end_date              as EndDate,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      booking_fee           as BookingFee,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      total_price           as TotalPrice,
      currency_code         as CurrencyCode,
      description           as Description,
      status                as TravelStatus,
      @Semantics.user.createdBy: true
      createdby            as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      createdat            as CreatedAt,
      @Semantics.user.lastChangedBy: true
      lastchangedby       as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      lastchangedat       as LastChangedAt,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,

      /* associations */
      _Booking,
      _Agency,
      _Customer,
      _Currency
}
