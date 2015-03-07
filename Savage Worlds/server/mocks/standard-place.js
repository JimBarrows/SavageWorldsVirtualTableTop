module.exports = function(app) {
  var express = require('express');
  var standardPlaceDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = [{id:1, name:'StandardPlace 1', description:'<p>StandardPlace 1 description goes here</p>'},
              {id:2, name:'StandardPlace 2', description:'<p>StandardPlace 2 description goes here.</p>'}]

  standardPlaceDescriptionRouter.get('/', function(req, res) {
    res.send({
      'standardPlace': data
    });
  });

  standardPlaceDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.standardPlace;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ standardPlace: newRec}).end();
  });

  standardPlaceDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'standardPlace':data[req.params.id -1]
    });
  });

  standardPlaceDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.standardPlaceDescription;  
    existingRecord.id = req.params.id;
    res.send({
      'standardPlace': existingRecord
    });
  });

  standardPlaceDescriptionRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/standardPlaces', standardPlaceDescriptionRouter);
};
