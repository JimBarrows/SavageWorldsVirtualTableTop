module.exports = function(app) {
  var express = require('express');
  var hindranceDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = []

  hindranceDescriptionRouter.get('/', function(req, res) {
    res.send({
      'hindrance': data
    });
  });

  hindranceDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.hindrance;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ hindrance: newRec}).end();
  });

  hindranceDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'hindrance':data[req.params.id -1]
    });
  });

  hindranceDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.hindranceDescription;  
    existingRecord.id = req.params.id;
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
