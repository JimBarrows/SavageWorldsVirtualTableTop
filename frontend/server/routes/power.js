module.exports = function(app) {
  var data = require('../data').data;
  var express = require('express');
  var powerRouter = express.Router();
  var bodyParser = require('body-parser');

  powerRouter.get('/', function(req, res) {
    res.send({
      'power': data.powers
    });
  });

  powerRouter.post('/', function(req, res) {
    var newRec = req.body.power;
    newRec.id = data.powers.length + 1;    
    data.powers.push( newRec);
    res.status(201).send({ power: newRec}).end();
  });

  powerRouter.get('/:id', function(req, res) {
    res.send({
      'power':data.powers[req.params.id -1]
    });
  });

  powerRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.power;  
    existingRecord.id = req.params.id;
    data[req.params.id -1] = existingRecord;
    res.send({
      'power': existingRecord
    });
  });

  powerRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/powers', powerRouter);
};
