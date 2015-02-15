module.exports = function(app) {
  var express = require('express');
  var plotPointsRouter = express.Router();
  var bodyParser = require('body-parser');
  var data = [{id:1, name:'The Fires of Escalon', description:'<p>The ravaging hordes are coming and a party of elven scouts must deny them sustenance.</p>'}]

  plotPointsRouter.get('/', function(req, res) {
    res.send({
      'plotPoints': data
    });
  });

  plotPointsRouter.post('/', function(req, res) {
    var newRec = eq.body.plotPoint;
    newRec.id = data.length + 1,      
    data.push( newRec);
    res.status(201).send({ plotPoint: newRec}).end();
  });

  plotPointsRouter.get('/:id', function(req, res) {
    res.send({
      'plot-points': data[req.params.id -1]
    });
  });

  plotPointsRouter.put('/:id', function(req, res) {
    var existingRecord = data[req.params.id -1];
    existingRecord = req.body.plotPoint;    
    existingRecord.id = req.params.id;
    res.send({
      'plot-points': existingRecord
    });
  });

  plotPointsRouter.delete('/:id', function(req, res) {
    data.splice(req.params.id - 1, 1);
    res.status(204).end();
  });

  app.use(bodyParser.json());
  app.use('/api/plotPoints', plotPointsRouter);
};
