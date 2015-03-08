module.exports = function(app) {
  var data = require('../data').data.standardRaces;
  var express = require('express');
  var standardRaceRouter = express.Router();
  var bodyParser = require('body-parser');

  standardRaceRouter.get('/', function(req, res) {
    res.send({
      'standard-race': data
    });
  });

  standardRaceRouter.post('/', function(req, res) {
    var newRec = req.body.standardRace;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ standardRace: newRec}).end();
  });

  standardRaceRouter.get('/:id', function(req, res) {
    res.send({
      'standard-race':data[req.params.id -1]
    });
  });

  standardRaceRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.standardRace;  
    existingRecord.id = req.params.id;
    res.send({
      'standard-race': existingRecord
    });
  });

  standardRaceRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/standardRaces', standardRaceRouter);
};
