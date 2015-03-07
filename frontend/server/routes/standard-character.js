module.exports = function(app) {
  var data = require('../data').data;
  var express = require('express');
  var standardCharacterRouter = express.Router();
  var bodyParser = require('body-parser');

  standardCharacterRouter.get('/', function(req, res) {
    res.send({
      'standardCharacter': data.standardCharacters
    });
  });

  standardCharacterRouter.post('/', function(req, res) {
    var newRec = req.body.standardCharacter;
    newRec.id = data.standardCharacters.length + 1;    
    data.standardCharacters.push( newRec);
    res.status(201).send({ standardCharacter: newRec}).end();
  });

  standardCharacterRouter.get('/:id', function(req, res) {
    res.send({
      'standardCharacter':data.standardCharacters[req.params.id -1]
    });
  });

  standardCharacterRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.standardCharacter;  
    existingRecord.id = req.params.id;
    data[req.params.id -1] = existingRecord;
    res.send({
      'standardCharacter': existingRecord
    });
  });

  standardCharacterRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/standardCharacters', standardCharacterRouter);
};
