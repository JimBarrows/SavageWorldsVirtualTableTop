/**
 * 
 */
package org.savageworlds.admin.rest;

import javax.ejb.EJB;
import javax.enterprise.context.RequestScoped;
import javax.validation.Valid;
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

import org.savageworlds.admin.dto.SkillDescriptionList;
import org.savageworlds.game.model.AttributeTypes;
import org.savageworlds.game.model.SkillDescription;
import org.savageworlds.repository.SkillDescriptionRepository;

/**
 * @author Jim
 *
 */
@RequestScoped
@Path("/skillDescriptions")
public class SkillDescriptionEndpoint {

	@EJB
	private SkillDescriptionRepository sdRepo;
	
	/**
	 * @param skilldescription
	 * @return
	 */
	@POST
	@Consumes("application/json")
	public Response create( SkillDescription skilldescription) {
		skilldescription = sdRepo.create(skilldescription); 
		return Response.created(UriBuilder.fromResource(SkillDescription.class).path(String.valueOf(skilldescription.getId())).build()).build();
	}

	/**
	 * @param id
	 * @return
	 */
	@GET
	@Path("/{id:[0-9][0-9]*}")
	@Produces("application/json")
	public Response findById(@PathParam("id") final Long id) {
		//TODO: retrieve the skilldescription 
		SkillDescription skilldescription = null;
		if (skilldescription == null) {
			return Response.status(Status.NOT_FOUND).build();
		}
		return Response.ok(skilldescription).build();
	}

	/**
	 * @param startPosition
	 * @param maxResult
	 * @return
	 */
	@GET
	@Produces("application/json")
	public SkillDescriptionList listAll(@QueryParam("start") final Integer startPosition, @QueryParam("max") final Integer maxResult) {
		//TODO: retrieve the skilldescriptions 
		final SkillDescriptionList skilldescriptions = new SkillDescriptionList();
		SkillDescription sd =  new SkillDescription();
		sd.setName("Test");
		sd.setId(1l);
		sd.setAttribute(AttributeTypes.Agility);
		sd.setDescription("Blah blah");
		skilldescriptions.add(sd );
		return skilldescriptions;
	}

	/**
	 * @param id
	 * @param skilldescription
	 * @return
	 */
	@PUT
	@Path("/{id:[0-9][0-9]*}")
	@Consumes("application/json")
	public Response update(@PathParam("id") Long id, final SkillDescription skilldescription) {
		//TODO: process the given skilldescription 
		return Response.noContent().build();
	}

	/**
	 * @param id
	 * @return
	 */
	@DELETE
	@Path("/{id:[0-9][0-9]*}")
	public Response deleteById(@PathParam("id") final Long id) {
		//TODO: process the skilldescription matching by the given id 
		return Response.noContent().build();
	}

}
