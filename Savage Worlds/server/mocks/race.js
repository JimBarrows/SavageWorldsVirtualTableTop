module.exports = function(app) {
  var express = require('express');
  var raceRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = [];

  raceRouter.get('/', function(req, res) {
    res.send({
      'race': data
    });
  });

  raceRouter.post('/', function(req, res) {
    var newRec = req.body.race;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ race: newRec}).end();
  });

  raceRouter.get('/:id', function(req, res) {
    res.send({
      'race':data[req.params.id -1]
    });
  });

  raceRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.race;  
    existingRecord.id = req.params.id;
    res.send({
      'race': existingRecord
    });
  });

  raceRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/races', raceRouter);
};
