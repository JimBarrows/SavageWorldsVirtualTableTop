module.exports = function(app) {
  var data = require('../data').data;
  var express = require('express');
  var archetypeDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');

  archetypeDescriptionRouter.get('/', function(req, res) {
    res.send({
      'archetype': data.archetypes
    });
  });

  archetypeDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.archetype;
    newRec.id = data.archetypes.length + 1,      
    data.archetypes.push( newRec);
    res.status(201).send({ archetype: newRec}).end();
  });

  archetypeDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'archetype':data.archetypes[req.params.id -1]
    });
  });

  archetypeDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data.archetypes[req.params.id -1];
    existingRecord = req.body.archetypeDescription;  
    existingRecord.id = req.params.id;
    res.send({
      'archetype': existingRecord
    });
  });

  archetypeDescriptionRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/archetypes', archetypeDescriptionRouter);
};
