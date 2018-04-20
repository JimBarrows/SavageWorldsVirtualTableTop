/**
 * Created by JimBarrows on 4/18/18.
 */
import rest from 'rest';
import defaultRequest from 'rest/interceptor/defaultRequest';
import pathPrefix from 'rest/interceptor/pathPrefix';

export default rest
		.wrap(pathPrefix, {prefix: 'http://localhost:8080'})
		.wrap(defaultRequest, {headers: {'Accept': 'application/hal+json'}});

