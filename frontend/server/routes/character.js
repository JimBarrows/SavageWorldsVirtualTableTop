module.exports = function(app) {
  var data = require('../data').data;
  var express = require('express');
  var characterRouter = express.Router();
  var bodyParser = require('body-parser');

  characterRouter.get('/', function(req, res) {
    res.send({
      'character': data.characters
    });
  });

  characterRouter.post('/', function(req, res) {
    var newRec = req.body.character;
    newRec.id = data.characters.length + 1;    
    data.characters.push( newRec);
    res.status(201).send({ character: newRec}).end();
  });

  characterRouter.get('/:id', function(req, res) {
    res.send({
      'character':data.characters[req.params.id -1]
    });
  });

  characterRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.character;  
    existingRecord.id = req.params.id;
    data[req.params.id -1] = existingRecord;
    res.send({
      'character': existingRecord
    });
  });

  characterRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/characters', characterRouter);
};
