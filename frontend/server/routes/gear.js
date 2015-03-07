module.exports = function(app) {
  var express = require('express');
  var gearDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = require('../data').data;

  gearDescriptionRouter.get('/', function(req, res) {
    res.send({
      'gear': data.gear
    });
  });

  gearDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.gear;
    newRec.id = data.gear.length + 1,      
    data.gear.push( newRec);
    res.status(201).send({ gear: newRec}).end();
  });

  gearDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'gear':data.gear[req.params.id -1]
    });
  });

  gearDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.gearDescription;  
    existingRecord.id = req.params.id;
    data[req.params.id -1] = existingRecord;
    res.send({
      'gear': existingRecord
    });
  });

  gearDescriptionRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/gears', gearDescriptionRouter);
};
