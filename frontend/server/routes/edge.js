module.exports = function(app) {
  var data = require('../data').data;
  var express = require('express');
  var edgeDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');

  edgeDescriptionRouter.get('/', function(req, res) {
    res.send({
      'edge': data.edges
    });
  });

  edgeDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.edge;
    newRec.id = data.edges.length + 1,      
    data.edges.push( newRec);
    res.status(201).send({ edge: newRec}).end();
  });

  edgeDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'edge':data.edges[req.params.id -1]
    });
  });

  edgeDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.edgeDescription;  
    existingRecord.id = req.params.id;
    res.send({
      'edge': existingRecord
    });
  });

  edgeDescriptionRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/edges', edgeDescriptionRouter);
};
