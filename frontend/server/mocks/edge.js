module.exports = function(app) {
  var express = require('express');
  var edgeDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = [{id:1, name:'Edge 1', description:'<p>Edge 1 description goes here</p>'},
              {id:2, name:'Edge 2', description:'<p>Edge 2 description goes here.</p>'}]

  edgeDescriptionRouter.get('/', function(req, res) {
    res.send({
      'edge': data
    });
  });

  edgeDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.edge;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ edge: newRec}).end();
  });

  edgeDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'edge':data[req.params.id -1]
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
