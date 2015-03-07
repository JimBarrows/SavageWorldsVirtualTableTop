module.exports = function(app) {
  var data = require('../data').data;
  var express = require('express');
  var standardExtraRouter = express.Router();
  var bodyParser = require('body-parser');

  standardExtraRouter.get('/', function(req, res) {
    res.send({
      'standardExtra': data.standardExtras
    });
  });

  standardExtraRouter.post('/', function(req, res) {
    var newRec = req.body.standardExtra;
    newRec.id = data.standardExtras.length + 1;    
    data.standardExtras.push( newRec);
    res.status(201).send({ standardExtra: newRec}).end();
  });

  standardExtraRouter.get('/:id', function(req, res) {
    res.send({
      'standardExtra':data.standardExtras[req.params.id -1]
    });
  });

  standardExtraRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.standardExtra;  
    existingRecord.id = req.params.id;
    data[req.params.id -1] = existingRecord;
    res.send({
      'standardExtra': existingRecord
    });
  });

  standardExtraRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/standardExtras', standardExtraRouter);
};
