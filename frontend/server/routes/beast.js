module.exports = function(app) {
  var data = require('../data').data;
  var express = require('express');
  var beastRouter = express.Router();
  var bodyParser = require('body-parser');

  beastRouter.get('/', function(req, res) {
    res.send({
      'beast': data.beasts
    });
  });

  beastRouter.post('/', function(req, res) {
    var newRec = req.body.beast;
    newRec.id = data.beasts.length + 1;    
    data.beasts.push( newRec);
    res.status(201).send({ beast: newRec}).end();
  });

  beastRouter.get('/:id', function(req, res) {
    res.send({
      'beast':data.beasts[req.params.id -1]
    });
  });

  beastRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.beast;  
    existingRecord.id = req.params.id;
    data[req.params.id -1] = existingRecord;
    res.send({
      'beast': existingRecord
    });
  });

  beastRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/beasts', beastRouter);
};
