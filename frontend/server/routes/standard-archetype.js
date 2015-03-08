module.exports = function(app) {
  var express = require('express');
  var standardArchetypeDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = require('../data').data.standardArchetypes;

  standardArchetypeDescriptionRouter.get('/', function(req, res) {
    res.send({
      'standardArchetype': data
    });
  });

  standardArchetypeDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.standardArchetype;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ standardArchetype: newRec}).end();
  });

  standardArchetypeDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'standardArchetype':data[req.params.id -1]
    });
  });

  standardArchetypeDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.standardArchetype;  
    existingRecord.id = req.params.id;
    data[req.params.id -1] = existingRecord;
    res.send({
      'standardArchetype': existingRecord
    });
  });

  standardArchetypeDescriptionRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/standardArchetypes', standardArchetypeDescriptionRouter);
};
