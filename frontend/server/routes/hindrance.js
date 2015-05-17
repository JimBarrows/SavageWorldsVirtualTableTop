module.exports = function(app) {
  var data = require('../data').data;
  var express = require('express');
  var hindranceDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');

  hindranceDescriptionRouter.get('/', function(req, res) {
    res.send({
      'hindrance': data.hindrances
    });
  });

  hindranceDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.hindrance;
    newRec.id = data.hindrances.length + 1,      
    data.hindrances.push( newRec);
    res.status(201).send({ hindrance: newRec}).end();
  });

  hindranceDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'hindrance':data.hindrances[req.params.id -1]
    });
  });

  hindranceDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data.hindrances[req.params.id -1];
    existingRecord = req.body.hindrance;  
    existingRecord.id = req.params.id;
    data.hindrances[req.params.id - 1] = existingRecord;
    res.send({
      'hindrance': existingRecord
    });
  });

  hindranceDescriptionRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/hindrances', hindranceDescriptionRouter);
};
