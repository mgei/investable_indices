const etfdb = require('etfdb-api');

// list all ETFs, sorted by year-to-date return, descending sort direction
etfdb
  .listEtfs((perPage = 2290), (page = 1), (sort = 'assets'), (order = 'desc'))
  .then(result => {
    console.log('Total ETFs:', result.meta.total_records);
    console.log('etf.symbol.text, etf.name.text, etf.mobile_title, etf.assets, etf.average_volume, etf.asset_class, etf.ytd');
    result.data.forEach(etf => console.log(etf.symbol.text, ';', etf.name.text, ';', etf.mobile_title, ';', etf.assets, ';', etf.average_volume, ';', etf.asset_class, ';', etf.ytd));
  });

// show first 15 holdings of TQQQ, sorted by weighting (DESC)
//etfdb.listHoldings('TQQQ').then(holdings => console.log(holdings));

// page 3 of TQQQ holdings
//etfdb.listHoldings('TQQQ', 3).then(holdings => console.log(holdings));
