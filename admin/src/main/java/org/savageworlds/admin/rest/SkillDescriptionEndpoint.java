/**
 * 
 */
package org.savageworlds.admin.rest;

import javax.ejb.EJB;
import javax.enterprise.context.RequestScoped;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;

import org.savageworlds.admin.dto.SkillDescriptionList;
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
	public SkillDescription create( SkillDescription skilldescription) {
		skilldescription = sdRepo.create(skilldescription); 
		return skilldescription;
	}

	/**
	 * @param id
	 * @return
	 */
	@GET
	@Path("/{id:[0-9][0-9]*}")
	@Produces("application/json")
	public SkillDescription findById(@PathParam("id") final Long id) {
		SkillDescription skill = sdRepo.findById( id);
		if( skill == null) {
			throw new NotFoundException();
		}
		return skill;
	}

	/**
	 * @param startPosition
	 * @param maxResult
	 * @return
	 */
	@GET
	@Produces("application/json")
	public SkillDescriptionList listAll(@QueryParam("start") final Integer startPosition, @QueryParam("max") final Integer maxResult) {
		
		return new SkillDescriptionList( sdRepo.findAll());
	}

	/**
	 * @param id
	 * @param skilldescription
	 * @return
	 */
	@PUT
	@Path("/{id:[0-9][0-9]*}")
	@Consumes("application/json")
	public SkillDescription update(@PathParam("id") Long id, final SkillDescription skilldescription) {
		if ((id == null) || (id < 0)) {
			throw new IllegalArgumentException("Id must be part of path, and greater than 0.");
		}
		skilldescription.setId(id);
		SkillDescription updated = sdRepo.update(skilldescription);
		return updated;
	}

	/**
	 * @param id
	 * @return
	 */
	@DELETE
	@Path("/{id:[0-9][0-9]*}")
	public void deleteById(@PathParam("id") final Long id) {
		
		sdRepo.delete( id);
	}

}
