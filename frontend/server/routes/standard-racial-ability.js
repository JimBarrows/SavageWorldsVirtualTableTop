module.exports = function(app) {
  var data = require('../data').data.standardRacialAbilities;
  var express = require('express');
  var standardRacialAbilitiesRouter = express.Router();
  var bodyParser = require('body-parser');

  standardRacialAbilitiesRouter.get('/', function(req, res) {
    res.send({
      'standard-racial-ability': data
    });
  });

  standardRacialAbilitiesRouter.post('/', function(req, res) {
    var newRec = req.body.standardRacialAbility;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ standardRacialAbility: newRec}).end();
  });

  standardRacialAbilitiesRouter.get('/:id', function(req, res) {
    res.send({
      'standard-racial-ability':data[req.params.id -1]
    });
  });

  standardRacialAbilitiesRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.standardRacialAbility;  
    existingRecord.id = req.params.id;
    res.send({
      'standard-racial-ability': existingRecord
    });
  });

  standardRacialAbilitiesRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/standardRacialAbilities', standardRacialAbilitiesRouter);
};
