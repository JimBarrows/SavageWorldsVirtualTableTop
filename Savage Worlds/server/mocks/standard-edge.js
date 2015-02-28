module.exports = function(app) {
  var express = require('express');
  var standardEdgeDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = [{id:1, name:'Edge 1', description:'<p>Edge 1 description goes here</p>'},
              {id:2, name:'Edge 2', description:'<p>Edge 2 description goes here.</p>'}]

  standardEdgeDescriptionRouter.get('/', function(req, res) {
    res.send({
      'standard-edge': data
    });
  });

  standardEdgeDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.standardEdgeDescription;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ standardEdgeDescription: newRec}).end();
  });

  standardEdgeDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'standard-edge':data[req.params.id -1]
    });
  });

  standardEdgeDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.standardEdgeDescription;  
    existingRecord.id = req.params.id;
    res.send({
      'standard-edge': existingRecord
    });
  });

  standardEdgeDescriptionRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/standardEdges', standardEdgeDescriptionRouter);
};
