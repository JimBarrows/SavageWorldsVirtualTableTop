module.exports = function(app) {
  var data = require('../data').data;
  var express = require('express');
  var standardBeastRouter = express.Router();
  var bodyParser = require('body-parser');

  standardBeastRouter.get('/', function(req, res) {
    res.send({
      'standardBeast': data.standardBeasts
    });
  });

  standardBeastRouter.post('/', function(req, res) {
    var newRec = req.body.standardBeast;
    newRec.id = data.standardBeasts.length + 1;    
    data.standardBeasts.push( newRec);
    res.status(201).send({ standardBeast: newRec}).end();
  });

  standardBeastRouter.get('/:id', function(req, res) {
    res.send({
      'standardBeast':data.standardBeasts[req.params.id -1]
    });
  });

  standardBeastRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.standardBeast;  
    existingRecord.id = req.params.id;
    data[req.params.id -1] = existingRecord;
    res.send({
      'standardBeast': existingRecord
    });
  });

  standardBeastRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/standardBeasts', standardBeastRouter);
};
