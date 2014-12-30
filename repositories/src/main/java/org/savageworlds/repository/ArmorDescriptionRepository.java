package org.savageworlds.repository;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import jdo.core.repository.RepositoryTemplate;

import org.savageworlds.game.model.ArmorDescription;

@Stateless
public class ArmorDescriptionRepository extends RepositoryTemplate<ArmorDescription, Long> {

	public ArmorDescriptionRepository() {
		super(ArmorDescription.class);		
	}

	@PersistenceContext(name = "SavageWorlds")
	protected EntityManager em;
	
	@Override
	protected EntityManager em() {
		return em;
	}

}
