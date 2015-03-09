module.exports = function(app) {
  var express = require('express');
  var standardEdgeRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = require('../data').data.standardEdges;

  standardEdgeRouter.get('/', function(req, res) {
    res.send({
      'standard-edge': data
    });
  });

  standardEdgeRouter.post('/', function(req, res) {
    var newRec = req.body.standardEdge;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ standardEdge: newRec}).end();
  });

  standardEdgeRouter.get('/:id', function(req, res) {
    res.send({
      'standard-edge':data[req.params.id -1]
    });
  });

  standardEdgeRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.standardEdge;  
    existingRecord.id = req.params.id;
    data[req.params.id -1] = existingRecord;
    res.send({
      'standard-edge': existingRecord
    });
  });

  standardEdgeRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/standardEdges', standardEdgeRouter);
};
