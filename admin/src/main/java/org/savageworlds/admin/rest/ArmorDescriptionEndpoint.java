package org.savageworlds.admin.rest;

import javax.ejb.EJB;
import javax.enterprise.context.RequestScoped;
import javax.ws.rs.Path;

import org.savageworlds.admin.dto.ArmorDescriptionList;
import org.savageworlds.game.model.ArmorDescription;
import org.savageworlds.repository.ArmorDescriptionRepository;

@RequestScoped
@Path("/armorDescriptions")
public class ArmorDescriptionEndpoint extends EndpointTemplate<ArmorDescription, ArmorDescriptionRepository, ArmorDescriptionList>{

	@EJB
	private ArmorDescriptionRepository repo;

	@Override
	protected ArmorDescriptionRepository repo() {		
		return repo;
	}

	@Override
	public ArmorDescriptionList listAll(Integer startPosition, Integer maxResult) {
		
		return new ArmorDescriptionList( repo.findAll());
	}
		
}
