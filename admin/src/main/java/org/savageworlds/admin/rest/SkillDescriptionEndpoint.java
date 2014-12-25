/**
 * 
 */
package org.savageworlds.admin.rest;

import javax.ejb.EJB;
import javax.enterprise.context.RequestScoped;
import javax.ws.rs.Path;

import org.savageworlds.admin.dto.SkillDescriptionList;
import org.savageworlds.game.model.SkillDescription;
import org.savageworlds.repository.SkillDescriptionRepository;

/**
 * @author Jim
 *
 */
@RequestScoped
@Path("/skillDescriptions")
public class SkillDescriptionEndpoint extends EndpointTemplate<SkillDescription, SkillDescriptionRepository, SkillDescriptionList>{

	@EJB
	private SkillDescriptionRepository repo;

	@Override
	protected SkillDescriptionRepository repo() {		
		return repo;
	}

	@Override
	public SkillDescriptionList listAll(Integer startPosition, Integer maxResult) {
		
		return new SkillDescriptionList( repo().findAll());
	}
		

}
