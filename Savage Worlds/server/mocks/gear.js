module.exports = function(app) {
  var express = require('express');
  var gearDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = []

  gearDescriptionRouter.get('/', function(req, res) {
    res.send({
      'gear': data
    });
  });

  gearDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.gear;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ gear: newRec}).end();
  });

  gearDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'gear':data[req.params.id -1]
    });
  });

  gearDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.gearDescription;  
    existingRecord.id = req.params.id;
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
