module.exports = function(app) {
  var data = require('../data').data;
  var express = require('express');
  var standardPowerRouter = express.Router();
  var bodyParser = require('body-parser');

  standardPowerRouter.get('/', function(req, res) {
    res.send({
      'standardPower': data.standardPowers
    });
  });

  standardPowerRouter.post('/', function(req, res) {
    var newRec = req.body.standardPower;
    newRec.id = data.standardPowers.length + 1;    
    data.standardPowers.push( newRec);
    res.status(201).send({ standardPower: newRec}).end();
  });

  standardPowerRouter.get('/:id', function(req, res) {
    res.send({
      'standardPower':data.standardPowers[req.params.id -1]
    });
  });

  standardPowerRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.standardPower;  
    existingRecord.id = req.params.id;
    data[req.params.id -1] = existingRecord;
    res.send({
      'standardPower': existingRecord
    });
  });

  standardPowerRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/standardPowers', standardPowerRouter);
};
