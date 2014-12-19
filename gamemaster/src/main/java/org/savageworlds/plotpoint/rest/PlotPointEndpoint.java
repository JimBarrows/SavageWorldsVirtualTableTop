/**
 * 
 */
package org.savageworlds.plotpoint.rest;

import javax.enterprise.context.RequestScoped;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriBuilder;

import org.savageworlds.plotpoint.dto.PlotPointList;
import org.savageworlds.plotpoint.model.PlotPoint;

/**
 * @author Jim
 *
 */
@RequestScoped
@Path("/plotPoints")
public class PlotPointEndpoint {

	/**
	 * @param plotpoint
	 * @return
	 */
	@POST
	@Consumes("application/json")
	public Response create(final PlotPoint plotpoint) {
		//TODO: process the given plotpoint 
		return Response.created(UriBuilder.fromResource(PlotPoint.class).path(String.valueOf(plotpoint.getId())).build()).build();
	}

	/**
	 * @param id
	 * @return
	 */
	@GET
	@Path("/{id:[0-9][0-9]*}")
	@Produces("application/json")
	public Response findById(@PathParam("id") final Long id) {
		//TODO: retrieve the plotpoint 
		PlotPoint plotpoint = null;
		if (plotpoint == null) {
			return Response.status(Status.NOT_FOUND).build();
		}
		return Response.ok(plotpoint).build();
	}

	/**
	 * @param startPosition
	 * @param maxResult
	 * @return
	 */
	@GET
	@Produces("application/json")
	public PlotPointList listAll(@QueryParam("start") final Integer startPosition, @QueryParam("max") final Integer maxResult) {		
		final PlotPointList plotpoints = new PlotPointList();
		return plotpoints;
	}

	/**
	 * @param id
	 * @param plotpoint
	 * @return
	 */
	@PUT
	@Path("/{id:[0-9][0-9]*}")
	@Consumes("application/json")
	public Response update(@PathParam("id") Long id, final PlotPoint plotpoint) {
		//TODO: process the given plotpoint 
		return Response.noContent().build();
	}

	/**
	 * @param id
	 * @return
	 */
	@DELETE
	@Path("/{id:[0-9][0-9]*}")
	public Response deleteById(@PathParam("id") final Long id) {
		//TODO: process the plotpoint matching by the given id 
		return Response.noContent().build();
	}

}
