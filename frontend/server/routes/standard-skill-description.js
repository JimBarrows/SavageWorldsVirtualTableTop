module.exports = function(app) {
  var express = require('express');
  var standardSkillDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = require('../data').data.standardSkills;

  standardSkillDescriptionRouter.get('/', function(req, res) {
    res.send({
      'standardSkillDescription': data
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
      'standardSkillDescription':data[req.params.id -1]
    });
  });

  standardSkillDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.standardSkillDescription;  
    existingRecord.id = req.params.id;
    data[req.params.id -1] = existingRecord;
    res.send({
      'standardSkillDescription': existingRecord
    });
  });

  standardSkillDescriptionRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/standardSkillDescriptions', standardSkillDescriptionRouter);
};
