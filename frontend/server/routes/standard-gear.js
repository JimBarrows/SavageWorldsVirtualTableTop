module.exports = function(app) {
  var express = require('express');
  var standardGearRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = require('../data').data.standardGear;
  
  standardGearRouter.get('/', function(req, res) {
    res.send({
      'standardGear': data
    });
  });

  standardGearRouter.post('/', function(req, res) {
    var newRec = req.body.standardGear;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ standardGear: newRec}).end();
  });

  standardGearRouter.get('/:id', function(req, res) {
    res.send({
      'standardGear':data[req.params.id -1]
    });
  });

  standardGearRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.standardGear;  
    existingRecord.id = req.params.id;
    data[req.params.id -1] = existingRecord;
    res.send({
      'standardGear': existingRecord
    });
  });

  standardGearRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/standardGears', standardGearRouter);
};
