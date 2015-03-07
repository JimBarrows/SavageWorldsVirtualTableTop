module.exports = function(app) {
  var express = require('express');
  var archetypeDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = [{id:1, name:'Archetype 1', description:'<p>Archetype 1 description goes here</p>'},
              {id:2, name:'Archetype 2', description:'<p>Archetype 2 description goes here.</p>'}]

  archetypeDescriptionRouter.get('/', function(req, res) {
    res.send({
      'archetype': data
    });
  });

  archetypeDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.archetype;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ archetype: newRec}).end();
  });

  archetypeDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'archetype':data[req.params.id -1]
    });
  });

  archetypeDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
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
