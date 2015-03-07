module.exports = function(app) {
  var data = require('../data').data;
  var express = require('express');
  var extraRouter = express.Router();
  var bodyParser = require('body-parser');

  extraRouter.get('/', function(req, res) {
    res.send({
      'extra': data.extras
    });
  });

  extraRouter.post('/', function(req, res) {
    var newRec = req.body.extra;
    newRec.id = data.extras.length + 1;    
    data.extras.push( newRec);
    res.status(201).send({ extra: newRec}).end();
  });

  extraRouter.get('/:id', function(req, res) {
    res.send({
      'extra':data.extras[req.params.id -1]
    });
  });

  extraRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.extra;  
    existingRecord.id = req.params.id;
    data[req.params.id -1] = existingRecord;
    res.send({
      'extra': existingRecord
    });
  });

  extraRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/extras', extraRouter);
};
