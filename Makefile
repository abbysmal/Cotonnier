RM = rm -f

NAME = Cotonnier

all: $(NAME)

$(NAME):
	  ghc Cotonnier.hs


clean:
	  $(RM) *.hi *.o *.aes

fclean: clean
	  $(RM) $(NAME)

re: fclean all
