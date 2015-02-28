module.exports = function(app) {
  var express = require('express');
  var standardGearDescriptionRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = [{id:1, name:'StandardGear 1', description:'<p>StandardGear 1 description goes here</p>'},
              {id:2, name:'StandardGear 2', description:'<p>StandardGear 2 description goes here.</p>'}]

  standardGearDescriptionRouter.get('/', function(req, res) {
    res.send({
      'standardGear': data
    });
  });

  standardGearDescriptionRouter.post('/', function(req, res) {
    var newRec = req.body.standardGear;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ standardGear: newRec}).end();
  });

  standardGearDescriptionRouter.get('/:id', function(req, res) {
    res.send({
      'standardGear':data[req.params.id -1]
    });
  });

  standardGearDescriptionRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.standardGearDescription;  
    existingRecord.id = req.params.id;
    res.send({
      'standardGear': existingRecord
    });
  });

  standardGearDescriptionRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/standardGears', standardGearDescriptionRouter);
};
