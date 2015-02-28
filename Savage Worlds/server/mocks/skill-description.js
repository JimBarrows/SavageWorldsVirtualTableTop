module.exports = function(app) {
  var express = require('express');
  var skillDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = []

  skillDescriptionRouter.get('/', function(req, res) {
    res.send({
      'skill-description': data
    });
  });

  skillDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.skillDescription;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ skillDescription: newRec}).end();
  });

  skillDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'skill-description':data[req.params.id -1]
    });
  });

  skillDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.skillDescription;  
    existingRecord.id = req.params.id;
    res.send({
      'skill-description': existingRecord
    });
  });

  skillDescriptionRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/skillDescriptions', skillDescriptionRouter);
};
