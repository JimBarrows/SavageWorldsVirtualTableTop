import DS from 'ember-data';

export default DS.RESTSerializer.extend(DS.EmbeddedRecordsMixin,{
	// primaryKey: '_id',
	attrs:{
		races: {serialize: 'records', deserialize: 'records'},
		skills: {serialize: 'records', deserialize: 'records'},
		edges: {serialize: 'records', deserialize: 'records'},
		hindrances: {serialize: 'records', deserialize: 'records'},
		gear:{serialize: 'records', deserialize: 'records'},
		places: {serialize: 'records', deserialize: 'records'}
	}	
});
