package org.savageworlds.vtt.virtualtabletopapi.repositories;

import org.savageworlds.vtt.virtualtabletopapi.models.Persona;
import org.springframework.data.repository.PagingAndSortingRepository;
import org.springframework.data.rest.core.annotation.RepositoryRestResource;

import java.util.List;

@RepositoryRestResource
public interface CharacterRepository extends PagingAndSortingRepository<Persona, Long> {
	List<Persona> findByName(String name);
}
