module.exports = function(app) {
  var express = require('express');
  var standardSkillDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = [{id:1, name:'Boating', description:'<p>Boating skill description goes here</p>', attribute:'Agility'},
              {id:2, name:'Fighting', description:'<p>Fighting description goes here.</p>', attribute:'Strength'}]

  standardSkillDescriptionRouter.get('/', function(req, res) {
    res.send({
      'standard-skill-description': data
    });
  });

  standardSkillDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.standardSkillDescription;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ standardSkillDescription: newRec}).end();
  });

  standardSkillDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'standard-skill-description':data[req.params.id -1]
    });
  });

  standardSkillDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.standardSkillDescription;  
    existingRecord.id = req.params.id;
    res.send({
      'standard-skill-description': existingRecord
    });
  });

  standardSkillDescriptionRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/standardSkillDescriptions', standardSkillDescriptionRouter);
};
