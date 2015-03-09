module.exports = function(app) {
  var express = require('express');
  var standardHindranceDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = require('../data').data.standardHindrances;

  standardHindranceDescriptionRouter.get('/', function(req, res) {
    res.send({
      'standardHindrance': data
    });
  });

  standardHindranceDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.standardHindrance;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ standardHindrance: newRec}).end();
  });

  standardHindranceDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'standardHindrance':data[req.params.id -1]
    });
  });

  standardHindranceDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.standardHindrance;  
    existingRecord.id = req.params.id;
    data[req.params.id -1] = existingRecord;
    res.send({
      'standardHindrance': existingRecord
    });
  });

  standardHindranceDescriptionRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/standardHindrances', standardHindranceDescriptionRouter);
};
